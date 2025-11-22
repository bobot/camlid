open Expr

(* untagging is considered as a kind of unboxing *)
type unbox_attribute = Unboxed | Untagged

type conv =
  | Boxed of { c2ml : code; ml2c : code }
  | Unboxable of {
      unbox_attribute : unbox_attribute;
      ucty : defined;
      ml2u : code;
      u2ml : code;
      u2c : code;
      c2u : code;
      u : var;
      c2ml : code;
      ml2c : code;
    }

type typedef = {
  mlname : string option;  (** ml name *)
  cty : defined;  (** print the c type *)
  mlty : defined;  (** print the ocaml type *)
  conv : conv;  (** convert C values of this type to ML value *)
  init : code option;
      (** Initialize values of this type before giving them to stub function *)
  init_expr : expr;  (** expression initialization of the c version *)
  free : code option;
      (** Free the C memory allocated during the call (not accessible in output
          OCaml value) *)
  v : var; (* variable for the addresse of ml version *)
  c : var; (* variable for the addresse of c version *)
}

type pinput =
  | PINone
  | PIBoxed of { ml : var; ml2c : code; pmlty : defined }
  | PIUnboxable of {
      unbox_attribute : unbox_attribute;
      ml2u : code;
      u2c : code;
      u : var;
      ml : var;
      pmlty : defined;
    }

type poutput =
  | PONone
  | POBoxed of { ml : var; c2ml : code; pmlty : defined }
  | POUnboxable of {
      unbox_attribute : unbox_attribute;
      u2ml : code;
      c2u : code;
      u : var;
      ml : var;
      c2ml : code; (* used when more than one result*)
      pmlty : defined;
    }

type param = {
  pinput : pinput;
  poutput : poutput;
  pused_in_call : (var * expr) option;
  pinit : code option;
  pinit_expr : (var * expr option) list;
  pfree : code option;
}

type result = {
  routput : poutput; (* appears in ML results *)
  rfree : code option;
  rc : var;
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

let ty_binds ?(binds = []) ?v ?c ty =
  let bv = match v with None -> [] | Some v -> [ (ty.v, v) ] in
  let bc = match c with None -> [] | Some c -> [ (ty.c, c) ] in
  bv @ bc @ binds
