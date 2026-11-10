(* Plans of schedule on many parameter combinations must pass Blocks.check.
   Their mutations are what the check must refuse; the survivors are printed by
   kind, to be read. *)
open Camlid
open Plan

let noalloc = { alloc = false; raising = false }
let allocates = { alloc = true; raising = false }

let base =
  {
    input = No_input;
    output = No_output;
    owned_by_ml = false;
    init = false;
    free = false;
    value_locals = false;
    ml_heap = false;
    ml2c = noalloc;
    c2ml = noalloc;
    in_check = false;
    out_check = false;
  }

let kinds =
  [
    ("scalar in", { base with input = Unboxed_input; output = No_output });
    ("custom in", { base with input = Boxed_input; owned_by_ml = true });
    ( "custom out",
      {
        base with
        output = Boxed_output;
        owned_by_ml = true;
        init = true;
        c2ml = allocates;
      } );
    ( "custom produced",
      { base with output = Boxed_output; owned_by_ml = true; c2ml = allocates }
    );
    ("string in", { base with input = Boxed_input; free = true });
    ( "string checked",
      { base with input = Boxed_input; free = true; in_check = true } );
    ( "string out",
      { base with output = Boxed_output; free = true; c2ml = allocates } );
    ( "checked out",
      {
        base with
        output = Boxed_output;
        free = true;
        c2ml = allocates;
        out_check = true;
      } );
    ("value in", { base with input = Boxed_input; ml_heap = true });
    ( "value array in",
      { base with input = Boxed_input; ml_heap = true; free = true } );
    ( "alloc in",
      { base with input = Boxed_input; free = true; ml2c = allocates } );
    ( "value out",
      {
        base with
        output = Boxed_output;
        value_locals = true;
        ml_heap = true;
        c2ml = allocates;
      } );
    ("scalar out", { base with output = Unboxable_output; c2ml = allocates });
    ( "inout",
      {
        base with
        input = Boxed_input;
        output = Boxed_output;
        free = true;
        c2ml = allocates;
      } );
  ]

let rec seqs n =
  if n = 0 then [ [] ]
  else
    List.concat_map (fun s -> List.map (fun k -> k :: s) kinds) (seqs (n - 1))

let options =
  List.concat_map
    (fun error ->
      List.map
        (fun release_lock -> { want_noalloc = None; release_lock; error })
        [ false; true ])
    [ false; true ]

let rec remove_nth n = function
  | [] -> []
  | _ :: r when n = 0 -> r
  | x :: r -> x :: remove_nth (n - 1) r

let rec swap_nth n = function
  | a :: b :: r when n = 0 -> b :: a :: r
  | a :: r -> a :: swap_nth (n - 1) r
  | [] -> []

let shrink_step = function
  | Check_in l -> List.mapi (fun i _ -> Check_in (remove_nth i l)) l
  | Ml2c l -> List.mapi (fun i _ -> Ml2c (remove_nth i l)) l
  | Init l -> List.mapi (fun i _ -> Init (remove_nth i l)) l
  | C2ml l -> List.mapi (fun i _ -> C2ml (remove_nth i l)) l
  | Free l -> List.mapi (fun i _ -> Free (remove_nth i l)) l
  | Check_error c ->
      List.mapi
        (fun i _ -> Check_error { c with frees = remove_nth i c.frees })
        c.frees
      @ List.mapi
          (fun i _ -> Check_error { c with moves = remove_nth i c.moves })
          c.moves
  | Check_out (l, c) ->
      List.mapi
        (fun i _ -> Check_out (l, { c with frees = remove_nth i c.frees }))
        c.frees
      @ List.mapi
          (fun i _ -> Check_out (l, { c with moves = remove_nth i c.moves }))
          c.moves
  | _ -> []

let step_name = function
  | Poll -> "Poll"
  | Check_in _ -> "Check_in"
  | Ml2c _ -> "Ml2c"
  | Init _ -> "Init"
  | Call _ -> "Call"
  | Check_error _ -> "Check_error"
  | Check_out _ -> "Check_out"
  | C2ml _ -> "C2ml"
  | Alloc_tuple _ -> "Alloc_tuple"
  | Free _ -> "Free"
  | Return -> "Return"

let is_empty_step = function
  | Check_in [] | Ml2c [] | Init [] | C2ml [] | Free [] | Check_out ([], _) ->
      true
  | _ -> false

let pp_l l = String.concat "," (List.map string_of_int l)

let pp_step s =
  let n = step_name s in
  match s with
  | Check_in l | Ml2c l | Init l | C2ml l | Free l | Alloc_tuple l ->
      Printf.sprintf "%s[%s]" n (pp_l l)
  | Call (l, r) ->
      Printf.sprintf "%s[%s]%s" n (pp_l l) (if r then "rel" else "")
  | Check_error c ->
      Printf.sprintf "%s(moves %s frees %s)" n (pp_l c.moves) (pp_l c.frees)
  | Check_out (l, c) ->
      Printf.sprintf "%s[%s](moves %s frees %s)" n (pp_l l) (pp_l c.moves)
        (pp_l c.frees)
  | Poll | Return -> n

let pp_plan (p : plan) =
  Printf.sprintf "%s | roots in=%s out=%s c=%s ret=%b err=%b%s"
    (String.concat " " (List.map pp_step p.steps))
    (pp_l p.roots.inputs) (pp_l p.roots.outputs) (pp_l p.roots.c_values)
    p.roots.ret p.roots.err
    (if p.noalloc then " noalloc" else "")

let mutations (p : plan) =
  let steps = List.filter (fun s -> not (is_empty_step s)) p.steps in
  let p = { p with steps } in
  let with_steps kind steps = (kind, { p with steps }) in
  List.concat
    [
      List.mapi
        (fun i s -> with_steps ("drop " ^ step_name s) (remove_nth i steps))
        steps;
      List.concat
        (List.mapi
           (fun i _ ->
             if i + 1 < List.length steps then
               [
                 with_steps
                   (Printf.sprintf "swap %s %s"
                      (step_name (List.nth steps i))
                      (step_name (List.nth steps (i + 1))))
                   (swap_nth i steps);
               ]
             else [])
           steps);
      List.concat
        (List.mapi
           (fun i s ->
             List.map
               (fun s' ->
                 with_steps
                   ("shrink " ^ step_name s)
                   (List.mapi (fun j x -> if i = j then s' else x) steps))
               (shrink_step s))
           steps);
      List.mapi
        (fun i _ ->
          ( "unroot input",
            {
              p with
              roots = { p.roots with inputs = remove_nth i p.roots.inputs };
            } ))
        p.roots.inputs;
      List.mapi
        (fun i _ ->
          ( "unroot output",
            {
              p with
              roots = { p.roots with outputs = remove_nth i p.roots.outputs };
            } ))
        p.roots.outputs;
      List.mapi
        (fun i _ ->
          ( "unroot c value",
            {
              p with
              roots = { p.roots with c_values = remove_nth i p.roots.c_values };
            } ))
        p.roots.c_values;
      (if p.roots.ret then
         [ ("unroot tuple", { p with roots = { p.roots with ret = false } }) ]
       else []);
      (if p.roots.err then
         [ ("unroot message", { p with roots = { p.roots with err = false } }) ]
       else []);
    ]

let () =
  let plans = ref 0
  and refused_sched = ref 0
  and bad = ref 0
  and muts = ref 0
  and killed = ref 0 in
  let survivors = Hashtbl.create 16 in
  let errors = Hashtbl.create 16 in
  let examples = Hashtbl.create 16 in
  List.iter
    (fun n ->
      List.iter
        (fun seq ->
          let ps = List.map snd seq in
          let call = List.init n Fun.id in
          List.iter
            (fun o ->
              match schedule ~name:"f" o ps ~call with
              | Stdlib.Error _ -> incr refused_sched
              | Stdlib.Ok p -> (
                  incr plans;
                  match Blocks.check o ps ~call p with
                  | Stdlib.Error e ->
                      incr bad;
                      let k = String.concat "," (List.map fst seq) in
                      if not (Hashtbl.mem errors e) then Hashtbl.add errors e k
                  | Stdlib.Ok () ->
                      List.iter
                        (fun (kind, m) ->
                          incr muts;
                          match Blocks.check o ps ~call m with
                          | Stdlib.Error _ -> incr killed
                          | Stdlib.Ok () ->
                              let c, ex =
                                Option.value
                                  ~default:
                                    ( 0,
                                      String.concat "; " (List.map fst seq)
                                      ^
                                      if o.release_lock then " +release"
                                      else if o.error then " +error"
                                      else "" )
                                  (Hashtbl.find_opt survivors kind)
                              in
                              if c = 0 then
                                Hashtbl.replace examples kind
                                  (Printf.sprintf
                                     "%s\n      was %s\n      now %s" ex
                                     (pp_plan p) (pp_plan m));
                              Hashtbl.replace survivors kind (c + 1, ex))
                        (mutations p)))
            options)
        (seqs n))
    (if Array.length Sys.argv > 1 then [ 0; 1; 2; 3 ] else [ 0; 1; 2 ]);
  (* The kinds of mutation accepted, all read and harmless: a root is useless
     when no GC follows, a free can move when nothing aliases, reordering
     independent conversions, and the destructor of what the failed C function
     did not produce. *)
  let harmless =
    [
      (* Poll is not needed for safety, only so that the signals are not late;
         run later, it is refused if the stub owns something *)
      "drop Poll";
      "swap Poll Call";
      "swap Poll Check_in";
      "swap Poll Ml2c";
      "shrink Check_error";
      "swap Alloc_tuple Free";
      "swap C2ml Free";
      "swap Ml2c Ml2c";
      "unroot c value";
      "unroot input";
      "unroot message";
      "unroot output";
      "unroot tuple";
    ]
  in
  let unexpected =
    Hashtbl.fold
      (fun k _ acc -> if List.mem k harmless then acc else k :: acc)
      survivors []
  in
  if Array.length Sys.argv > 1 || !bad > 0 || unexpected <> [] then (
    Printf.printf "plans=%d refused by schedule=%d REFUSED BY CHECK=%d\n" !plans
      !refused_sched !bad;
    Hashtbl.iter (fun e k -> Printf.printf "  check: %s (e.g. %s)\n" e k) errors;
    Printf.printf "mutations=%d refused=%d survivors=%d\n" !muts !killed
      (!muts - !killed);
    List.iter
      (fun (k, (c, _)) ->
        Printf.printf "  survive %-28s %d\n    %s\n" k c
          (Hashtbl.find examples k))
      (List.sort compare
         (Hashtbl.fold (fun k c acc -> (k, c) :: acc) survivors [])));
  if !bad > 0 || unexpected <> [] then exit 1
