(** The plan of a stub: the order of its steps and the values registered with
    the GC. [schedule] makes every decision of safety, the printer
    ({!Expert.code_c_fun}) only translates the steps into C. It is independent
    of the expressions, so that it can be modelled and proved in Why3 (see
    [agents/design/safe-stubs.md]). The parameters are numbered by their
    position. *)

type input = No_input | Boxed_input | Unboxed_input
type output = No_output | Boxed_output | Unboxable_output

type param = {
  input : input;
  output : output;
  init : bool;  (** has a constructor, used when it is not an input *)
  free : bool;  (** has a destructor *)
  on_error : bool;  (** is moved into an OCaml value on the error path *)
  value_locals : bool;  (** has C locals of type [value] *)
  ml_heap : bool;  (** its C argument is or points into an OCaml value *)
  noalloc_in : bool;  (** the conversion of its input doesn't allocate *)
  noalloc_out : bool;  (** the boxed conversion of its output doesn't allocate *)
}

type options = { noalloc : bool option; release_lock : bool; error : bool }

type result =
  | Unit_result
  | One_value of int  (** a boxed result *)
  | One_unboxed of int  (** an unboxed result, returned as a C value *)
  | Tuple of int list  (** the outputs, stored in a tuple *)

type roots = {
  inputs : int list;  (** boxed inputs, [CAMLparam] *)
  outputs : int list;  (** boxed outputs, [CAMLlocal] *)
  ret : bool;  (** the tuple, [CAMLlocal] with the outputs *)
  err : bool;  (** the error message, [CAMLlocal] *)
  c_values : int list;  (** parameters with C locals of type [value] *)
}

type step =
  | Ml2c of int list
  | Init of int list
  | Enter_blocking
  | Call of int list
  | Leave_blocking
  | Check_error of { moves : int list; frees : int list }
      (** if the C function failed: create the message, move the outputs owned
          by OCaml, free, then raise *)
  | C2ml of int list
  | Alloc_tuple of int list
  | Free of int list
  | Return

type plan = {
  noalloc : bool;  (** no allocation: no roots, [[@@noalloc]] *)
  result : result;
  roots : roots;
  steps : step list;
}

let no_roots =
  { inputs = []; outputs = []; ret = false; err = false; c_values = [] }

let schedule ~name (o : options) (params : param list) ~(call : int list) =
  let params = List.mapi (fun i p -> (i, p)) params in
  let pids f =
    List.filter_map (fun (i, p) -> if f p then Some i else None) params
  in
  let ml_heap =
    List.exists (fun (i, p) -> p.ml_heap && List.mem i call) params
  in
  if o.release_lock && ml_heap then
    Error
      (Printf.sprintf
         "Camlid: the runtime lock can't be released in %s, an argument of the \
          C function points into the OCaml heap"
         name)
  else
    let outputs = pids (fun p -> p.output <> No_output) in
    let result =
      match outputs with
      | [] -> Unit_result
      | [ i ] -> (
          match (List.assoc i params).output with
          | Unboxable_output -> One_unboxed i
          | _ -> One_value i)
      | l -> Tuple l
    in
    let can_noalloc =
      (not o.release_lock) && (not o.error)
      && List.length outputs <= 1
      && List.for_all
           (fun (_, p) ->
             p.noalloc_in
             && (p.noalloc_out
                || (List.length outputs = 1 && p.output = Unboxable_output)))
           params
    in
    let noalloc =
      match o.noalloc with
      | None -> Ok can_noalloc
      | Some false -> Ok false
      | Some true when can_noalloc -> Ok true
      | Some true ->
          Error
            (Printf.sprintf
               "Camlid: %s can't be noalloc, a conversion allocates or the \
                function has many results, an error check or releases the lock"
               name)
    in
    match noalloc with
    | Error _ as e -> e
    | Ok noalloc ->
        let roots =
          if noalloc then no_roots
          else
            {
              inputs = pids (fun p -> p.input = Boxed_input);
              outputs =
                (match result with
                | Unit_result | One_unboxed _ -> []
                | One_value i -> [ i ]
                | Tuple l -> l);
              ret = (match result with Tuple _ -> true | _ -> false);
              err = o.error;
              c_values = pids (fun p -> p.value_locals);
            }
        in
        let frees = pids (fun p -> p.free) in
        let steps =
          List.concat
            [
              [
                Ml2c (pids (fun p -> p.input <> No_input));
                Init (pids (fun p -> p.init));
              ];
              (if o.release_lock then [ Enter_blocking ] else []);
              [ Call call ];
              (if o.release_lock then [ Leave_blocking ] else []);
              (if o.error then
                 [ Check_error { moves = pids (fun p -> p.on_error); frees } ]
               else []);
              [ C2ml outputs ];
              (match result with Tuple l -> [ Alloc_tuple l ] | _ -> []);
              [ Free frees; Return ];
            ]
        in
        Ok { noalloc; result; roots; steps }
