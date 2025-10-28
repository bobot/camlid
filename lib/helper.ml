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

let ref ty =
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
