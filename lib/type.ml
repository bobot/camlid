open Expr

type typedef = {
  descr : string;
  mlname : string option;  (** ml name *)
  cty : code;  (** print the c type *)
  mlty : code;  (** print the ocaml type *)
  c2ml : code;  (** convert C values of this type to ML value *)
  ml2c : code;  (** ml2c *)
  init : code;
      (** Initialize values of this type before giving them to stub function *)
  init_expr : expr;  (** expression initialization *)
  free : code;
      (** Free the C memory allocated during the call (not accessible in output
          OCaml value) *)
  v : var; (* variable for the ml version *)
  c : var; (* variable for the c version *)
}

type param = {
  input : bool; (* appears in ML parameters and converted before call *)
  output : bool; (* appears in ML results and converted after call *)
  used_in_call : bool; (* appears in the stubbed C call parameters *)
  pty : typedef;
  pc : var;
  binds : (Var.t * expr) list;
}

type result = {
  routput : bool; (* appears in ML results and converted after call *)
  rty : typedef;
  rc : var;
  binds : (Var.t * expr) list;
}

type conf = Expr.expr list

let code ?(kind = C) ?params ?keep_name ?(locals = []) ?(ovars = [])
    ?(ret = expr "void") name p =
  Format.kdprintf
    (fun k ->
      let id = ID.mk ?keep_name name in
      let params =
        match params with
        | None ->
            let params = params_of_expr k in
            let params = List.sort_uniq Var.compare (ovars @ params) in
            let local = Var.S.of_list locals in
            let params =
              List.filter (fun v -> not (Var.S.mem v local)) params
            in
            params
        | Some params -> params
      in
      let pp_args fmt (var : var) =
        Fmt.pf fmt "%a %a" var.ty.expr () pp_var var
      in
      mk ~kind ~params id (fun fmt () ->
          Fmt.pf fmt "@[<hv 2>@[static %a %a(%a){@]@ %t@ @[};@]@]@." ret.expr ()
            pp_id id
            Fmt.(list ~sep:comma pp_args)
            params k))
    p

let codef ?(kind = C) ?params ?keep_name ?(locals = []) ?(ovars = [])
    ?(ret = expr "void") name pp =
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
  let pp_args fmt (var : var) = Fmt.pf fmt "%a %a" var.ty.expr () pp_var var in
  mk ~kind ~params id (fun fmt () ->
      Fmt.pf fmt "@[<hv 2>@[static %a %a(%a){@]@ %t@ @[};@]@]@." ret.expr ()
        pp_id id
        Fmt.(list ~sep:comma pp_args)
        params p)

let c2ml ?(binds = []) ~v ~c () fmt ty =
  pp_call fmt (ty.c2ml, [ (ty.v, v); (ty.c, c) ] @ binds)

let ml2c ?(binds = []) ~v ~c () fmt ty =
  pp_call fmt (ty.ml2c, [ (ty.v, v); (ty.c, c) ] @ binds)

let init ?(binds = []) ~c () fmt ty =
  pp_call fmt (ty.init, [ (ty.c, c) ] @ binds)

let free ?(binds = []) ~c () fmt ty =
  pp_call fmt (ty.free, [ (ty.c, c) ] @ binds)

let init_expr fmt ty = Fmt.pf fmt "%a" ty.init_expr.expr ()
