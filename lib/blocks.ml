(** The memory of a stub, at a low level, as an executable semantics.

    There are only blocks of memory: the C variables of the stub, the C memory,
    the OCaml blocks. A block is valid or not, has an owner, and {e mentions}
    other blocks (the pointers it contains). A stub is a list of operations
    ({!type:op}), run on this state by {!run}. What happens:

    - a GC ({!gc}) moves every block owned by OCaml, and collects those not
      reachable from the roots. A block that mentions a moved block becomes
      invalid, unless the GC updates it: a root, or a block owned by OCaml. C
      memory owned by blocks dies when all its owners are dead.

    The rules:

    - use only what is valid and reaches only valid blocks;
    - what a GC keeps is valid and mentions only valid blocks;
    - free only what the stub owns, and own nothing when leaving, by return or
      by exception, nor when something can raise;
    - when the stub leaves, the OCaml caller can drop everything but the result:
      a last GC checks that the result keeps alive what it mentions;
    - a [noalloc] stub has no GC and doesn't raise.

    Releasing the runtime lock has no rule of its own: the C function has copies
    of its arguments, which are not registered, and a GC can run while it reads
    them.

    The GC is the worst case, it moves and collects all it can. So one run
    decides the safety of the given operations.

    Trusted: the operations describe what the C code does, see {!of_plan}. *)

module S = Set.Make (String)
module M = Map.Make (String)

(** Who is responsible for a block *)
type owner =
  | Frame  (** a C variable of the stub, gone when it leaves *)
  | Stub  (** C memory the stub must free, or give away, before leaving *)
  | Ocaml  (** in the OCaml heap: moved and collected by the GC *)
  | Of of string list
      (** C memory freed when these OCaml blocks are all dead: one finalizer, or
          a reference count shared by several *)

type state = {
  owners : owner M.t;
  freed : S.t;
  valid : S.t;
  mentions : S.t M.t;
  roots : S.t;  (** registered with the GC: [CAMLparam], [CAMLlocal] *)
  facts : S.t;  (** the checks done *)
  noalloc : bool;
}

type op =
  | Use of string list  (** the blocks are read *)
  | Set of string * string list  (** the block now mentions these blocks *)
  | Copy of string * string
      (** [Copy (dst, src)]: [dst] now mentions the same *)
  | New of string * owner * string list  (** a new valid block *)
  | New_uninit of string * owner
      (** a new block not initialized yet ([caml_alloc_small]): {!Set} or
          {!Fill} makes it valid *)
  | Fill of string * string list
      (** adds mentions to a block and makes it valid *)
  | Give of string * owner  (** the stub gives a block it owns *)
  | Free of string
      (** the destructor. It accepts an initial value: a block never created. *)
  | Forget of string  (** the block was not created after all *)
  | Gc
  | May_raise
  | Mark of string  (** a check was done *)
  | Need of string  (** this check must have been done *)
  | Branch of op list  (** a path that leaves the stub *)
  | Leave of string option  (** return this block, or raise *)

let mentions st b = Option.value ~default:S.empty (M.find_opt b st.mentions)
let is_valid st b = S.mem b st.valid
let owner st b = M.find_opt b st.owners
let is_ocaml st b = owner st b = Some Ocaml

let owned st =
  M.fold
    (fun b o acc -> if o = Stub then S.add b acc else acc)
    st.owners S.empty

(** From the roots through the blocks the GC scans: its heap *)
let reachable st =
  let rec go seen = function
    | [] -> seen
    | b :: todo ->
        let next =
          S.filter
            (fun m -> is_ocaml st m && not (S.mem m seen))
            (mentions st b)
        in
        go (S.union seen next) (S.elements next @ todo)
  in
  go st.roots (S.elements st.roots)

let gc st =
  let alive = reachable st in
  let survives b =
    match owner st b with
    | Some Ocaml -> S.mem b alive
    | Some (Of l) -> List.exists (fun k -> S.mem k alive) l
    | Some (Frame | Stub) | None -> true
  in
  let updated b = S.mem b st.roots || is_ocaml st b in
  let keeps_valid b =
    survives b && (updated b || not (S.exists (is_ocaml st) (mentions st b)))
  in
  let st' = { st with valid = S.filter keeps_valid st.valid } in
  let dead b = not (is_valid st' b) in
  let kept =
    S.filter
      (fun b ->
        is_ocaml st b || (S.mem b st.roots && not (S.is_empty (mentions st b))))
      alive
  in
  match S.find_first_opt dead kept with
  | Some b -> Error (b ^ " is kept by a GC but not initialized")
  | None -> (
      match
        S.choose_opt (S.filter (fun b -> S.exists dead (mentions st' b)) kept)
      with
      | Some b ->
          Error
            (Printf.sprintf "%s is kept by a GC but mentions %s, dead" b
               (S.choose (S.filter dead (mentions st' b))))
      | None -> Ok st')

let ( let* ) = Result.bind

(** Valid, and reaches only valid blocks *)
let use st b =
  let rec go seen = function
    | [] -> Ok st
    | x :: todo ->
        if not (is_valid st x) then
          Error
            (if x = b then b ^ " is used but is not valid"
             else Printf.sprintf "%s is used but reaches %s, not valid" b x)
        else
          let next = S.diff (mentions st x) seen in
          go (S.union seen next) (S.elements next @ todo)
  in
  go (S.singleton b) [ b ]

let rec step st op =
  match op with
  | Use l ->
      List.fold_left
        (fun st b ->
          let* st = st in
          use st b)
        (Ok st) l
  | Set (b, l) ->
      Ok
        {
          st with
          valid = S.add b st.valid;
          mentions = M.add b (S.of_list l) st.mentions;
        }
  | Copy (dst, src) ->
      Ok
        {
          st with
          valid = S.add dst st.valid;
          mentions = M.add dst (mentions st src) st.mentions;
        }
  | New (b, o, l) ->
      if M.mem b st.owners then Error (b ^ " is created twice")
      else
        Ok
          {
            st with
            owners = M.add b o st.owners;
            freed = S.remove b st.freed;
            valid = S.add b st.valid;
            mentions = M.add b (S.of_list l) st.mentions;
          }
  | New_uninit (b, o) ->
      if M.mem b st.owners then Error (b ^ " is created twice")
      else
        Ok
          {
            st with
            owners = M.add b o st.owners;
            freed = S.remove b st.freed;
            mentions = M.add b S.empty st.mentions;
          }
  | Fill (b, l) ->
      Ok
        {
          st with
          valid = S.add b st.valid;
          mentions = M.add b (S.union (mentions st b) (S.of_list l)) st.mentions;
        }
  | Give (b, o) ->
      if not (S.mem b (owned st)) then
        Error ("give " ^ b ^ ", not owned by the stub")
      else Ok { st with owners = M.add b o st.owners }
  | Free b -> (
      if S.mem b st.freed then Error ("double free of " ^ b)
      else
        match owner st b with
        | None -> Ok st
        | Some Stub ->
            Ok
              {
                st with
                valid = S.remove b st.valid;
                owners = M.remove b st.owners;
                freed = S.add b st.freed;
              }
        | Some _ -> Error ("free " ^ b ^ ", not owned by the stub"))
  | Forget b ->
      Ok { st with valid = S.remove b st.valid; owners = M.remove b st.owners }
  | Gc -> if st.noalloc then Error "a noalloc stub allocates" else gc st
  | May_raise -> (
      if st.noalloc then Error "a noalloc stub raises"
      else
        match S.choose_opt (owned st) with
        | Some b -> Error ("can raise while the stub owns " ^ b)
        | None -> Ok st)
  | Mark c -> Ok { st with facts = S.add c st.facts }
  | Need c ->
      if S.mem c st.facts then Ok st else Error ("check " ^ c ^ " not done")
  | Branch l ->
      let* _ = run_from st l in
      Ok st
  | Leave result -> (
      match S.choose_opt (owned st) with
      | Some b -> Error ("the stub leaves and still owns " ^ b)
      | None ->
          let* st = match result with None -> Ok st | Some b -> use st b in
          let roots =
            match result with None -> S.empty | Some b -> S.singleton b
          in
          let* _ = gc { st with roots } in
          Ok st)

and run_from st = function
  | [] -> Ok st
  | op :: l ->
      let* st = step st op in
      run_from st l

(** Runs the operations. The blocks of [slots] are C variables, not valid until
    assigned, except those given in [init] with what they mention. *)
let run ?(noalloc = false) ?(roots = []) ?(init = []) ?(blocks = []) ~slots ops
    =
  let st =
    {
      owners = M.empty;
      freed = S.empty;
      valid = S.empty;
      mentions = M.empty;
      roots = S.of_list roots;
      facts = S.empty;
      noalloc;
    }
  in
  let declare st s =
    let st = { st with owners = M.add s Frame st.owners } in
    match List.assoc_opt s init with
    | Some l ->
        {
          st with
          valid = S.add s st.valid;
          mentions = M.add s (S.of_list l) st.mentions;
        }
    | None -> st
  in
  let st = List.fold_left declare st slots in
  let* st =
    List.fold_left
      (fun st (b, o, l) ->
        let* st = st in
        step st (New (b, o, l)))
      (Ok st) blocks
  in
  let* _ = run_from st ops in
  Ok ()

(** {1 The operations of a plan}

    What the printer generates ([Expert.code_c_fun_of_plan]) and what the C
    templates do, given their contract ({!Plan.param}). The names of the blocks,
    for the register (parameter) [i]: the variables [C i] (C value), [In i],
    [Out i] (OCaml), [Loc i] (C locals of type [value]), [Arg i] (the copy seen
    by a C function with the lock released); the blocks [MlIn i], [MlOut i],
    [Own i] (C memory freed by the destructor), [Data i] (C memory stored in the
    OCaml block). *)

open Plan

let name s i = s ^ string_of_int i
let c = name "C"
let in_ = name "In"
let out = name "Out"
let loc = name "Loc"
let arg = name "Arg"
let ml_in = name "MlIn"
let ml_out = name "MlOut"
let own = name "Own"
let data = name "Data"
let ml_c = name "MlC"

(** The aliasing of the C function, not checked by camlid: the OCaml output [i]
    keeps alive the OCaml inputs [keeps i] ([Expert.keep_alive], [with_parent])
*)
let of_plan ?(keeps = fun _ -> []) ~(options : options) (ps : param list)
    (p : plan) :
    string list
    * string list
    * (string * string list) list
    * (string * owner * string list) list
    * op list =
  let ps = Array.of_list ps in
  let n = Array.length ps in
  let regs = List.init n Fun.id in
  let boxed i = ps.(i).input = Boxed_input in
  let has_in i = ps.(i).input <> No_input in
  let produced i = (not (has_in i)) && not ps.(i).init in
  let locals i = ps.(i).value_locals in
  (* The blocks a C value mentions *)
  let cblocks i =
    let q = ps.(i) in
    (if q.free then [ own i ] else [])
    @ (if q.owned_by_ml then [ data i ] else [])
    @ (if q.ml_heap && boxed i then [ ml_in i ] else [])
    @ if q.ml_heap && produced i then [ ml_c i ] else []
  in
  (* New blocks when the stub produces the C value *)
  let fresh i =
    let q = ps.(i) in
    (if q.free then [ New (own i, Stub, []) ] else [])
    @ (if q.owned_by_ml then [ New (data i, Stub, []) ] else [])
    @ if q.ml_heap && produced i then [ New (ml_c i, Ocaml, []) ] else []
  in
  let c_used i = c i :: (if locals i then [ loc i ] else []) in
  let out_effects i =
    match p.result with One_unboxed j when j = i -> pure | _ -> ps.(i).c2ml
  in
  let ml2c i =
    let q = ps.(i) in
    (if q.in_check then [ Need (name "in" i) ] else [])
    @ (if boxed i then [ Use [ in_ i ] ] else [])
    @ (if q.ml2c.raising then [ May_raise ] else [])
    @ (if q.ml2c.alloc then [ Gc ] else [])
    @ (if q.free then [ New (own i, Stub, []) ] else [])
    @ [ Set (c i, cblocks i) ]
  in
  let check_in i =
    (if boxed i then [ Use [ in_ i ] ] else [])
    @ List.concat_map
        (fun j -> if selected S_pre_input ps.(j) then [ Use [ c j ] ] else [])
        regs
    @ [ May_raise; Mark (name "in" i) ]
  in
  let init i =
    List.concat_map (fun j -> if has_in j then [ Use [ c j ] ] else []) regs
    @ fresh i
    @ [ Set (c i, cblocks i) ]
  in
  let call l release =
    let args =
      List.concat_map
        (fun i ->
          (if produced i then [] else [ c i ])
          @ if locals i then [ loc i ] else [])
        l
    in
    (* The C function reads copies of its arguments while a GC runs: when the
       lock is released, or when it writes an OCaml value, so allocates. In the
       second case it is responsible for the values it is given. *)
    let copies filter =
      let l = List.filter (fun i -> (not (produced i)) && filter i) l in
      List.map (fun i -> Copy (arg i, c i)) l @ [ Gc; Use (List.map arg l) ]
    in
    let writes_value =
      List.exists (fun i -> produced i && ps.(i).ml_heap) regs
    in
    let during =
      if release then copies (fun _ -> true)
      else if writes_value then copies (fun i -> not ps.(i).ml_heap)
      else []
    in
    (Use args :: during)
    @ List.concat_map
        (fun i -> if produced i then fresh i @ [ Set (c i, cblocks i) ] else [])
        regs
    @ [ Mark "called" ]
  in
  let c2ml ~checked i =
    let q = ps.(i) in
    let e = out_effects i in
    (if checked then
       [ Need "called" ]
       @ (if options.error then [ Need "error" ] else [])
       @ if q.out_check then [ Need (name "out" i) ] else []
     else [])
    @ [ Use (c_used i) ]
    @ (if e.raising then [ May_raise ] else [])
    @ (if e.alloc then [ Gc ] else [])
    @ [
        New
          ( ml_out i,
            Ocaml,
            (if q.owned_by_ml then [ data i ] else [])
            @ List.map ml_in (keeps i) );
      ]
    @ (if q.owned_by_ml then [ Give (data i, Of [ ml_out i ]) ] else [])
    @ [ Set (out i, [ ml_out i ]); Mark (name "made" i) ]
  in
  let free i : op = Free (own i) in
  let cleanup { moves; frees } =
    [ Gc; New ("MlMsg", Ocaml, []); Set ("Msg", [ "MlMsg" ]) ]
    @ List.concat_map (c2ml ~checked:false) moves
    @ List.map free frees
    @ [ Use [ "Msg" ]; Leave None ]
  in
  let revert =
    List.concat_map
      (fun i ->
        if produced i then
          (if ps.(i).free then [ Forget (own i) ] else [])
          @ if ps.(i).owned_by_ml then [ Forget (data i) ] else []
        else [])
      regs
  in
  let check_out i =
    (if options.error then [ Need "error" ] else [])
    @ [ Need "called"; Use (c_used i); Mark (name "out" i) ]
  in
  let step : step -> op list = function
    | Poll -> [ May_raise; Gc ]
    | Check_in l -> List.concat_map check_in l
    | Ml2c l -> List.concat_map ml2c l
    | Init l -> List.concat_map init l
    | Call (l, release) -> call l release
    | Check_error cl ->
        [ Need "called"; Branch (revert @ cleanup cl); Mark "error" ]
    | Check_out ([], _) -> []
    | Check_out (l, cl) ->
        Branch (List.concat_map check_out l @ cleanup cl)
        :: List.concat_map check_out l
    | C2ml l -> List.concat_map (c2ml ~checked:true) l
    | Alloc_tuple l ->
        [ Gc; New_uninit ("MlTuple", Ocaml); Set ("Tup", [ "MlTuple" ]) ]
        @ [
            Use (List.map out l);
            Fill ("MlTuple", List.map ml_out l);
            Mark "tuple";
          ]
    | Free l -> List.map free l
    | Return ->
        (* every output is converted: a CAMLlocal is valid before that, as
           Val_unit *)
        List.concat_map
          (fun i ->
            if ps.(i).output <> No_output then [ Need (name "made" i) ] else [])
          regs
        @ (match p.result with Tuple _ -> [ Need "tuple" ] | _ -> [])
        @ [
            Leave
              (match p.result with
              | Unit_result | One_unboxed _ -> None
              | One_value i -> Some (out i)
              | Tuple _ -> Some "Tup");
          ]
  in
  let roots =
    List.map in_ p.roots.inputs
    @ List.map out p.roots.outputs
    @ (if p.roots.ret then [ "Tup" ] else [])
    @ (if p.roots.err then [ "Msg" ] else [])
    @ List.concat_map
        (fun i ->
          (if ps.(i).ml_heap then [ c i ] else [])
          @ if locals i then [ loc i ] else [])
        p.roots.c_values
  in
  let slots =
    [ "Tup"; "Msg" ]
    @ List.concat_map (fun i -> [ c i; in_ i; out i; loc i; arg i ]) regs
  in
  let starts_valid =
    (* OCaml arguments; C locals of type value; CAMLlocal start as Val_unit *)
    List.concat_map
      (fun i ->
        (if boxed i then [ (in_ i, [ ml_in i ]) ]
         else if has_in i then [ (in_ i, []) ]
         else [])
        @ if locals i then [ (loc i, []) ] else [])
      regs
    @ List.map
        (fun r -> (r, []))
        (List.map out p.roots.outputs
        @ (if p.roots.ret then [ "Tup" ] else [])
        @ if p.roots.err then [ "Msg" ] else [])
  in
  let blocks =
    List.concat_map
      (fun i ->
        if boxed i then
          if ps.(i).owned_by_ml then
            [ (data i, Of [ ml_in i ], []); (ml_in i, Ocaml, [ data i ]) ]
          else [ (ml_in i, Ocaml, []) ]
        else [])
      regs
  in
  (roots, slots, starts_valid, blocks, List.concat_map step p.steps)

(** The C function is called once, with the requested arguments, and its result
    is checked when asked *)
let shape (o : options) call (p : plan) =
  let rec go called checked = function
    | [] -> false
    | Call (l, release) :: r ->
        (not called) && l = call && release = o.release_lock
        && go true checked r
    | Check_error _ :: r -> called && go called true r
    | [ Return ] -> called && ((not o.error) || checked)
    | Return :: _ -> false
    | _ :: r -> go called checked r
  in
  go false false p.steps

(** The plan is safe: to be run on each stub generated *)
let check ?keeps (o : options) (ps : param list) ~call (p : plan) =
  if not (shape o call p) then
    Stdlib.Error "the C function is not called as asked"
  else if
    (o.want_noalloc = Some true && not p.noalloc)
    || (o.want_noalloc = Some false && p.noalloc)
  then Stdlib.Error "noalloc is not as asked"
  else
    let roots, slots, init, blocks, ops = of_plan ?keeps ~options:o ps p in
    if p.noalloc && roots <> [] then
      Stdlib.Error "a noalloc stub registers values"
    else run ~noalloc:p.noalloc ~roots ~init ~blocks ~slots ops
