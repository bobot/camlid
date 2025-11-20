open Expr

type typedef = {
  mlname : string option;  (** ml name *)
  cty : defined;  (** print the c type *)
  mlty : defined;  (** print the ocaml type *)
  c2ml : code;  (** convert C values of this type to ML value *)
  ml2c : code;  (** ml2c *)
  init : code option;
      (** Initialize values of this type before giving them to stub function *)
  init_expr : expr;  (** expression initialization *)
  free : code option;
      (** Free the C memory allocated during the call (not accessible in output
          OCaml value) *)
  v : var; (* variable for the ml version *)
  c : var; (* variable for the c version *)
}

type param = {
  pinput : var option;
  poutput : var option;
  pused_in_call : expr option;
  pc2ml : code option;
  pml2c : code option;
  pinit : code option;
  pinit_expr : expr;
  pfree : code option;
  pc : var; (* c variable *)
  pmlty : defined;
}

type result = {
  routput : var option; (* appears in ML results *)
  rc2ml : code option;
  rfree : code option;
  rc : var;
  rmlty : defined;
}

type conf = Expr.expr list

let codef ?(kind = C) ?params ?keep_name ?(locals = []) ?(ovars = [])
    ?(ret = expr "void") ?(doc = Fmt.nop) name pp =
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
  let c =
    mk ~kind ~params id (fun fmt () ->
        Fmt.pf fmt "%a@[<hv 2>@[static %a %a(%a){@]@ %t@ @[};@]@]@." doc ()
          ret.expr () pp_id id
          Fmt.(list ~sep:comma pp_args)
          params p)
  in
  c

let codefo ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name pp =
  let k = fun fmt -> pp { fmt = (fun p -> Fmt.pf fmt p) } in
  if expr_is_empty k then None
  else Some (codef ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name pp)

let code ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name p =
  Format.kdprintf
    (fun k ->
      codef ?kind ?params ?keep_name ?locals ?ovars ?ret ?doc name
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

let c2ml ?(binds = []) ~v ~c () fmt ty =
  pp_calli fmt (ty.c2ml, [ (ty.v, v); (ty.c, c) ] @ binds)

let ml2c ?(binds = []) ~v ~c () fmt ty =
  pp_calli fmt (ty.ml2c, [ (ty.v, v); (ty.c, c) ] @ binds)

let init ?(binds = []) ~c () fmt ty =
  match ty.init with
  | None -> ()
  | Some f -> pp_calli fmt (f, [ (ty.c, c) ] @ binds)

let free ?(binds = []) ~c () fmt ty =
  match ty.free with
  | None -> ()
  | Some f -> pp_calli fmt (f, [ (ty.c, c) ] @ binds)

let init_expr fmt ty = Fmt.pf fmt "%a" ty.init_expr.expr ()
