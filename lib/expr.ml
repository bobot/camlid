type code = { id : id; kind : kind; toplevel : expr; params : var list }
(** Allows for a piece of text/code to depend on parameters, bind parameters,
    and depend on the avalaibility of other code *)

and expr = { expr : unit Fmt.t }
and kind = C | ML | H

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

module StringH = Hashtbl.Make (String)

type env = {
  ids_c : string ID.H.t;
  string_ids_c : int StringH.t;
  ids_ml : string ID.H.t;
  string_ids_ml : int StringH.t;
  vars : string Var.H.t;
  string_vars : int StringH.t;
}

let create_env () =
  {
    ids_c = ID.H.create 16;
    string_ids_c = StringH.create 10;
    ids_ml = ID.H.create 16;
    string_ids_ml = StringH.create 10;
    vars = Var.H.create 16;
    string_vars = StringH.create 10;
  }

module PPGenID (H : Hashtbl.S) = struct
  let pp ids string_ids prefix id id_name id_keep_name =
    match H.find_opt ids id with
    | Some s -> s
    | None ->
        let rec aux basename i =
          let s = Printf.sprintf "%s%i" basename i in
          match StringH.find_opt string_ids s with
          | None ->
              StringH.add string_ids s 0;
              StringH.replace string_ids basename i;
              s
          | Some _ -> aux basename (i + 1)
        in
        let find basename =
          match StringH.find_opt string_ids basename with
          | Some i -> aux basename (i + 1)
          | None ->
              StringH.add string_ids basename 0;
              basename
        in
        let s =
          if id_keep_name then (
            if StringH.mem string_ids id_name then
              Format.eprintf
                "%s is already used but it is asked to keep its name" id_name
            else StringH.add string_ids id_name 0;
            id_name)
          else find (Printf.sprintf "%s%s" prefix id_name)
        in
        H.add ids id s;
        s
end

module PPID = PPGenID (ID.H)
module PPVar = PPGenID (Var.H)

let run ~kind ~prefix env expr =
  let b = Buffer.create 16 in
  let q = Queue.create () in
  let fmt = Format.formatter_of_buffer b in
  let pp_id =
    match kind with
    | C | H ->
        fun fmt id ->
          Fmt.string fmt
            (PPID.pp env.ids_c env.string_ids_c prefix id id.name id.keep_name)
    | ML ->
        fun fmt id ->
          Fmt.string fmt
            (PPID.pp env.ids_ml env.string_ids_ml prefix id id.name id.keep_name)
  in
  let pp_var fmt id =
    Fmt.string fmt (PPVar.pp env.vars env.string_vars "" id id.name false)
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

let toplevel ?(kind = C) id p =
  Format.kdprintf
    (fun k ->
      let params = params_of_expr k in
      mk ~kind ~params id (fun fmt () -> k fmt))
    p

let final_print ~prefix ~ml ~c ~h kind expr =
  let open struct
    type t = Seen | Printed
  end in
  let env = create_env () in
  let printed = ID.H.create 16 in
  let rec aux kind expr =
    Var.H.clear env.vars;
    StringH.clear env.string_vars;
    let buf, codes = run ~kind ~prefix env expr in
    Queue.iter aux_code codes;
    match kind with
    | ML -> Buffer.output_buffer ml buf
    | C -> Buffer.output_buffer c buf
    | H -> Buffer.output_buffer h buf
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
