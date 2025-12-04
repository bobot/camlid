type defined =
  | Def of definition
      (** Allows for a piece of text/code to depend on parameters, and depend on
          the avalaibility of other code *)
  | DImplicit of (defined * code list)

and definition = { id : id; kind : kind; toplevel : expr }
and code = { defined : defined; params : var list }
and expr = { expr : unit Fmt.t }
and kind = C | ML | H
and id = { id : int; name : string; keep_name : bool }
and var = { id : int; name : string; ty : expr }

let expr p = Format.kdprintf (fun k -> { expr = (fun fmt () -> k fmt) }) p
let pp_expr fmt e = e.expr fmt ()
let dimplicit defined others = DImplicit (defined, others)

module ID = struct
  type t = id

  let hash (t : t) = t.id
  let equal t1 t2 = t1 == t2
  let compare (t1 : t) (t2 : t) = Int.compare t1.id t2.id
  let c = ref (-1)

  let mk ?(keep_name = false) name : t =
    incr c;
    { id = !c; name; keep_name }

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

  let mk name ty : var =
    incr c;
    { id = !c; name; ty }

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

type env_binding = (Var.t * expr) list

type Format.stag +=
  | Dep of definition
  | PrintID of ID.t
  | PrintVar of Var.t
  | Bind of Var.t * expr
  | InEnv of env_binding

let bind_expr var ve e =
  expr "%a%a%a" Format.pp_open_stag
    (Bind (var, ve))
    pp_expr e Format.pp_close_stag ()

let binds l e = List.fold_left (fun acc (v, e) -> bind_expr v e acc) e l
let pp_expr_binds fmt (e, bs) = pp_expr fmt (binds bs e)
let def_of_code = fun { defined; _ } -> defined

let rec def_of_def = function
  | Def def -> def
  | DImplicit (def, _) -> def_of_def def

let name_of_def x = (def_of_def x).id.name

let open_close_stag fmt t =
  Format.pp_open_stag fmt t;
  Format.pp_close_stag fmt ()

let pp_dep fmt c = open_close_stag fmt (Dep c)
let pp_id fmt id = open_close_stag fmt (PrintID id)

let rec pp_def fmt def =
  let rec aux = function
    | Def def ->
        pp_dep fmt def;
        open_close_stag fmt (PrintID def.id)
    | DImplicit (def, others) ->
        List.iter (code_dep fmt) others;
        aux def
  in
  aux def

and def_dep fmt = function
  | Def def -> pp_dep fmt def
  | DImplicit (def, others) ->
      List.iter (code_dep fmt) others;
      def_dep fmt def

and code_dep fmt = function { defined; _ } -> def_dep fmt defined

let e_def code = { expr = Fmt.const pp_def code }
let pp_var fmt c = open_close_stag fmt (PrintVar c)
let e_var var = { expr = Fmt.const pp_var var }
let e_addr v = expr "&%a" pp_var v
let e_deref v = expr "*%a" pp_var v

let pp_call fmt (code, binds) =
  let aux binds = function
    | { defined; params } ->
        let pp_arg fmt v =
          match List.assq_opt v binds with
          | None -> pp_var fmt v
          | Some { expr } -> expr fmt ()
        in
        Fmt.pf fmt "@[<hv>@[<hv 2>@[%a(@]@,%a@]@,)@]" pp_def defined
          Fmt.(list ~sep:comma pp_arg)
          params
  in
  aux binds code

let pp_calli fmt (c, b) = Fmt.pf fmt "%a;" pp_call (c, b)
let pp_call_ret fmt (ret, c, b) = Fmt.pf fmt "%a=%a;" ret.expr () pp_call (c, b)

let def ?(kind = C) id toplevel =
  Def { id; kind; toplevel = { expr = toplevel } }

let ddef ?kind id p =
  Format.kdprintf (fun k -> def ?kind id (fun fmt () -> k fmt)) p

let mk ?(kind = C) ?(params = []) id toplevel =
  let code =
    { defined = Def { id; kind; toplevel = { expr = toplevel } }; params }
  in
  code

type fp = { fmt : 'a. ('a, Format.formatter, unit) format -> 'a }

let fp ?kind ?params id pp =
  mk ?kind ?params id (fun fmt () -> pp { fmt = (fun p -> Fmt.pf fmt p) })

let dfp ?kind ?params id =
  Format.kdprintf (fun k -> mk ?kind ?params id (fun fmt () -> k fmt))

module StringH = Hashtbl.Make (struct
  include String

  let hash (x : string) = Hashtbl.hash x
end)

type env = {
  ids_c : string ID.H.t;
  string_ids_c : int StringH.t;
  ids_ml : string ID.H.t;
  string_ids_ml : int StringH.t;
  vars : string Var.H.t;
  string_vars : int StringH.t;
  bindings : env_binding Stack.t;
}

let current_bindings env = Option.value ~default:[] (Stack.top_opt env.bindings)

let create_env () =
  {
    ids_c = ID.H.create 16;
    string_ids_c = StringH.create 10;
    ids_ml = ID.H.create 16;
    string_ids_ml = StringH.create 10;
    vars = Var.H.create 16;
    string_vars = StringH.create 10;
    bindings = Stack.create ();
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
              (* todo, iterate first to find all the keep name *) ()
            else StringH.add string_ids id_name 0;
            id_name)
          else find (Printf.sprintf "%s%s" prefix id_name)
        in
        H.add ids id s;
        s
end

module PPID = PPGenID (ID.H)
module PPVar = PPGenID (Var.H)

let run_aux ~pp_id env e fmt =
  let q = Queue.create () in
  let free_vars = Var.H.create 10 in
  let pp_var fmt id =
    match List.assq_opt id (current_bindings env) with
    | None ->
        if not (Var.H.mem free_vars id) then Var.H.add free_vars id ();
        Fmt.string fmt (PPVar.pp env.vars env.string_vars "" id id.name false)
    | Some expr -> pp_expr fmt expr
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
        | InEnv binds -> Stack.push binds env.bindings
        | Bind (var, e) ->
            let binds = current_bindings env in
            let e =
              expr "%a%a%a" Format.pp_open_stag (InEnv binds) pp_expr e
                Format.pp_close_stag ()
            in
            Stack.push ((var, e) :: binds) env.bindings
        | _ -> ());
      print_close_stag =
        (function
        | InEnv _ -> ignore (Stack.pop env.bindings)
        | Bind _ -> ignore (Stack.pop env.bindings)
        | _ -> ());
    };
  e fmt;
  Format.pp_print_flush fmt ();
  (q, free_vars)

let run ~kind ~prefix env e =
  let b = Buffer.create 16 in
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
  let q, _ = run_aux ~pp_id env (fun fmt -> pp_expr fmt e) fmt in
  (b, q)

let params_of_expr expr =
  let fmt = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let env = create_env () in
  let _, free_vars = run_aux ~pp_id:(fun _ _ -> ()) env expr fmt in
  let l = List.sort Var.compare @@ List.of_seq @@ Var.H.to_seq_keys free_vars in
  l

exception Expr_is_not_empty

let expr_is_empty expr =
  let print_sub s start len =
    for i = start to len - 1 do
      match s.[i] with ' ' | '\n' -> () | _ -> raise Expr_is_not_empty
    done
  in
  let fmt = Format.make_formatter print_sub (fun () -> ()) in
  Format.pp_set_tags fmt true;
  Format.pp_set_print_tags fmt true;
  Format.pp_set_formatter_stag_functions fmt
    {
      mark_open_stag = (fun _ -> "");
      mark_close_stag = (fun _ -> "");
      print_open_stag =
        (function
        | Dep _ -> ()
        | PrintID _ -> raise Expr_is_not_empty
        | PrintVar _ -> raise Expr_is_not_empty
        | _ -> ());
      print_close_stag = (fun _ -> ());
    };
  match expr fmt with () -> true | exception Expr_is_not_empty -> false

let toplevel_callable ?(kind = C) id p =
  Format.kdprintf
    (fun k ->
      let params = params_of_expr k in
      mk ~kind ~params id (fun fmt () -> k fmt))
    p

let toplevel ?(kind = C) id p =
  Format.kdprintf (fun k -> def ~kind id (fun fmt () -> k fmt)) p

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

let codef ?(inline = false) ?(kind = C) ?params ?keep_name ?(locals = [])
    ?(ovars = []) ?(ret = expr "void") ?(doc = Fmt.nop) name pp =
  let p = fun fmt -> pp { fmt = (fun p -> Fmt.pf fmt p) } in
  let id = ID.mk ?keep_name name in
  let params =
    match params with
    | None ->
        let params = params_of_expr p in
        let params = List.sort_uniq Var.compare (ovars @ params) in
        let local = Var.S.of_list locals in
        let params = List.filter (fun v -> not (Var.S.mem v local)) params in
        params
    | Some params -> params
  in
  let pp_inline fmt b = if b then Fmt.pf fmt "inline " in
  let pp_args fmt (var : var) = Fmt.pf fmt "%a %a" pp_expr var.ty pp_var var in
  let c =
    mk ~kind ~params id (fun fmt () ->
        Fmt.pf fmt "%a@[<hv 2>@[static %a%a %a(%a){@]@ %t@ @[}@]@]@." doc ()
          pp_inline inline pp_expr ret pp_id id
          Fmt.(list ~sep:comma pp_args)
          params p)
  in
  c

let codefo ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name pp =
  let k = fun fmt -> pp { fmt = (fun p -> Fmt.pf fmt p) } in
  if expr_is_empty k then None
  else Some (codef ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name pp)

let code ?inline ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name p =
  Format.kdprintf
    (fun k ->
      codef ?inline ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name
        (fun { fmt } -> fmt "%t" k))
    p

let codeo ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name p =
  Format.kdprintf
    (fun k ->
      if expr_is_empty k then None
      else
        Some
          (codef ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name
             (fun { fmt } -> fmt "%t" k)))
    p

let call_codef ?locals name binds f =
  let id = codef name ?locals f in
  expr "%a" pp_calli (id, binds)

let expro s =
  Format.kdprintf
    (fun k ->
      if expr_is_empty k then None
      else Some { expr = (fun fmt () -> Fmt.pf fmt "%t" k) })
    s
