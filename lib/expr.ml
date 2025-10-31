type code = { id : id; kind : kind; toplevel : expr; params : var list }
(** Allows for a piece of text/code to depend on parameters, bind parameters,
    and depend on the avalaibility of other code *)

and expr = { expr : unit Fmt.t }
and kind = C | ML

and id = {
  id : int;
  name : string;
  args : id list;
  mutable code : code option;
  keep_name : bool;
}

and var = { id : int; name : string; args : var list; ty : expr }

let expr p = Format.kdprintf (fun k -> { expr = (fun fmt () -> k fmt) }) p

module ID = struct
  type t = id

  let hash (t : t) = t.id
  let equal t1 t2 = t1 == t2
  let compare (t1 : t) (t2 : t) = Int.compare t1.id t2.id
  let c = ref (-1)

  let mk ?(keep_name = false) ?(args = []) name : t =
    incr c;
    { id = !c; name; args; code = None; keep_name }

  module H = Hashtbl.Make (struct
    type nonrec t = t

    let hash = hash
    let equal = equal
  end)

  module S = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module Var = struct
  type t = var

  let hash (t : t) = t.id
  let equal t1 t2 = t1 == t2
  let compare (t1 : t) (t2 : t) = Int.compare t1.id t2.id
  let c = ref (-1)

  let mk ?(args = []) name ty : var =
    incr c;
    { id = !c; name; args; ty }

  module H = Hashtbl.Make (struct
    type nonrec t = t

    let hash = hash
    let equal = equal
  end)

  module S = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

type Format.stag += Dep of code | PrintID of ID.t | PrintVar of Var.t

let open_close_stag fmt t =
  Format.pp_open_stag fmt t;
  Format.pp_close_stag fmt ()

let pp_dep fmt c = open_close_stag fmt (Dep c)
let pp_id fmt id = open_close_stag fmt (PrintID id)

let pp_code fmt code =
  pp_dep fmt code;
  open_close_stag fmt (PrintID code.id)

let pp_var fmt c = open_close_stag fmt (PrintVar c)

let pp_call fmt (code, bind) =
  open_close_stag fmt (Dep code);
  List.iter
    (fun (v, _) ->
      if not (List.memq v code.params) then
        Fmt.failwith "Spurious binder %s in %s call" v.name code.id.name)
    bind;
  let pp_arg fmt v =
    match List.assq_opt v bind with
    | None -> pp_var fmt v
    | Some { expr } -> expr fmt ()
  in
  Fmt.pf fmt "@[<hv>@[<hv 2>@[%a(@]@,%a@]@,)@]" pp_id code.id
    Fmt.(list ~sep:comma pp_arg)
    code.params

let mk ?(kind = C) ?(params = []) id toplevel =
  let code = { id; kind; params; toplevel = { expr = toplevel } } in
  id.code <- Some code;
  code

(* let nop name = mk Fmt.nop
let any s = mk (Fmt.any s) *)

type fp = { fmt : 'a. ('a, Format.formatter, unit) format -> 'a }

let fp ?kind ?params id pp =
  mk ?kind ?params id (fun fmt () -> pp { fmt = (fun p -> Fmt.pf fmt p) })

let dpr = Format.dprintf

let dfp ?kind ?params id =
  Format.kdprintf (fun k -> mk ?kind ?params id (fun fmt () -> k fmt))

type env = { ids : string ID.H.t; vars : string Var.H.t }

let run ~prefix env expr =
  let b = Buffer.create 16 in
  let q = Queue.create () in
  let fmt = Format.formatter_of_buffer b in
  let pp_id fmt id =
    let s =
      match ID.H.find_opt env.ids id with
      | Some s -> s
      | None ->
          let s =
            if id.keep_name then id.name
            else Printf.sprintf "%s_%s%i" prefix id.name (ID.H.length env.ids)
          in
          ID.H.add env.ids id s;
          s
    in
    Fmt.string fmt s
  in
  let pp_var fmt id =
    let s =
      match Var.H.find_opt env.vars id with
      | Some s -> s
      | None ->
          let s = Printf.sprintf "%s%i" id.name (Var.H.length env.vars) in
          Var.H.add env.vars id s;
          s
    in
    Fmt.string fmt s
  in
  Format.pp_set_tags fmt true;
  Format.pp_set_print_tags fmt true;
  Format.pp_set_formatter_stag_functions fmt
    {
      mark_open_stag = (fun _ -> "");
      mark_close_stag = (fun _ -> "");
      print_open_stag =
        (function
        | Dep c -> Queue.push c q
        | PrintID id -> pp_id fmt id
        | PrintVar v -> pp_var fmt v
        | _ -> ());
      print_close_stag = (fun _ -> ());
    };
  expr.expr fmt ();
  Format.pp_print_flush fmt ();
  (b, q)

let params_of_expr expr =
  let h = Var.H.create 10 in
  let fmt = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  Format.pp_set_tags fmt true;
  Format.pp_set_print_tags fmt true;
  Format.pp_set_formatter_stag_functions fmt
    {
      mark_open_stag = (fun _ -> "");
      mark_close_stag = (fun _ -> "");
      print_open_stag =
        (function
        | Dep _ -> ()
        | PrintID _ -> ()
        | PrintVar v -> if not (Var.H.mem h v) then Var.H.add h v ()
        | _ -> ());
      print_close_stag = (fun _ -> ());
    };
  expr fmt;
  let l = List.sort Var.compare @@ List.of_seq @@ Var.H.to_seq_keys h in
  l

let toplevel id p =
  Format.kdprintf
    (fun k ->
      let params = params_of_expr k in
      mk ~kind:C ~params id (fun fmt () -> k fmt))
    p

let final_print ~prefix ~ml ~c kind expr =
  let open struct
    type t = Seen | Printed
  end in
  let env = { ids = ID.H.create 16; vars = Var.H.create 16 } in
  let printed = ID.H.create 16 in
  let rec aux kind expr =
    let buf, codes = run ~prefix env expr in
    Queue.iter aux_code codes;
    match kind with
    | ML -> Buffer.output_buffer ml buf
    | C -> Buffer.output_buffer c buf
  and aux_code c =
    match ID.H.find_opt printed c.id with
    | None ->
        ID.H.add printed c.id Seen;
        aux c.kind c.toplevel;
        ID.H.add printed c.id Printed
    | Some Seen -> failwith "Cycle in code"
    | Some Printed -> ()
  in
  aux kind expr
