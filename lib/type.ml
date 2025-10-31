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
  v : var; (* variable for the ml version *)
  c : var; (* variable for the c version *)
}

type param = {
  input : bool; (* appears in ML parameters and converted before call *)
  output : bool; (* appears in ML results and converted after call *)
  used_in_call : bool; (* appears in the stubbed C call parameters *)
  pty : typedef;
  pc : var;
}

type result = {
  routput : bool; (* appears in ML results and converted after call *)
  rty : typedef;
  rc : var;
}

type func = { fname : string; params : param list; result : result option }

let stub_name fmt f = Fmt.pf fmt "camlid_fun_%s" f.fname

type decl = Fun of func
type conf = decl list

let code ?keep_name ?(locals = []) ?(ovars = []) ?(ret = "void") name p =
  Format.kdprintf
    (fun k ->
      let id = ID.mk ?keep_name name in
      let params = params_of_expr k in
      let params = List.sort_uniq Var.compare (ovars @ params) in
      let local = Var.S.of_list locals in
      let params = List.filter (fun v -> not (Var.S.mem v local)) params in
      let pp_args fmt (var : var) =
        Fmt.pf fmt "%a %a" var.ty.expr () pp_var var
      in
      mk ~kind:C ~params id (fun fmt () ->
          Fmt.pf fmt "@[<hv 2>@[static %s %a(%a){@]@ %t@ @[}@]@]@." ret pp_id id
            Fmt.(list ~sep:comma pp_args)
            params k))
    p

let codef ?keep_name ?(ovars = []) ?(ret = "void") name pp =
  let p = fun fmt -> pp { fmt = (fun p -> Fmt.pf fmt p) } in
  let id = ID.mk ?keep_name name in
  let params = params_of_expr p in
  let params = List.sort_uniq Var.compare (ovars @ params) in
  let pp_args fmt (var : var) = Fmt.pf fmt "%a %a" var.ty.expr () pp_var var in
  mk ~kind:C ~params id (fun fmt () ->
      Fmt.pf fmt "@[<hv 2>@[static %s %a(%a){@]@ %t@ @[}@]@]@." ret pp_id id
        Fmt.(list ~sep:comma pp_args)
        params p)

let c2ml ~v ~c fmt ty = pp_call fmt (ty.c2ml, [ (ty.v, v); (ty.c, c) ])
let ml2c ~v ~c fmt ty = pp_call fmt (ty.ml2c, [ (ty.v, v); (ty.c, c) ])
let init ~c fmt ty = pp_call fmt (ty.init, [ (ty.c, c) ])
let init_expr fmt ty = Fmt.pf fmt "%a" ty.init_expr.expr ()
