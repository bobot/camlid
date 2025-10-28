open Type

let int =
  {
    name = "int";
    descr = "int";
    deps = [];
    cty = Fmt.any "int";
    mlty = Fmt.any " = int";
    mlname = "camlid_ml_int";
    c2ml = Fmt.any "(value * v, int * x){ *v = Val_int(*x); }";
    ml2c = Fmt.any "(int * x, value * v){ *x = Int_val(*v); }";
    init = Fmt.any "(int * x){ }";
    init_expr = Fmt.any "0";
  }

let ptr_ref ty =
  {
    name = "ref_" ^ ty.name;
    descr = "ref on " ^ ty.descr;
    deps = [ ty ];
    cty = (fun fmt () -> Fmt.pf fmt "%a *" cty ty);
    mlty = (fun fmt () -> Fmt.pf fmt "= %a" mlty ty);
    mlname = "camlid_ml_ptr_" ^ ty.mlname;
    c2ml =
      (fun fmt () ->
        Fmt.pf fmt "(value * v, %a ** x){ %a(v,*x); }" cty ty c2ml ty);
    ml2c =
      (fun fmt () ->
        Fmt.pf fmt "(%a ** x, value * v){ %a(*x,v); }" cty ty ml2c ty);
    init = Fmt.any "(int ** x){ }";
    init_expr =
      (fun fmt () ->
        Fmt.pf fmt "&(((struct { %a a; }) { %a }).a)" cty ty ty.init_expr ());
  }

let input ty name = { input = true; output = false; pty = ty; pname = name }
let output ty name = { input = false; output = true; pty = ty; pname = name }
let inout ty name = { input = true; output = true; pty = ty; pname = name }
let ignored ty name = { input = false; output = false; pty = ty; pname = name }

let func fname ?result ?ignored_result params =
  match (result, ignored_result) with
  | Some _, Some _ ->
      failwith "Camlid.Helper.func: can't set both result and ignored_result"
  | Some rty, None ->
      Fun { fname; params; result = Some { rty; routput = true } }
  | None, Some rty ->
      Fun { fname; params; result = Some { rty; routput = false } }
  | None, None -> Fun { fname; params; result = None }
