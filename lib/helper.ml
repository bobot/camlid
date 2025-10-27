let int =
  Type.
    {
      name = "int";
      descr = "int";
      deps = [];
      cty = Fmt.any "int";
      mlty = Fmt.any "int";
      c2ml = Fmt.any "(value * v, int * x){ *v = Val_int(*x); }";
      ml2c = Fmt.any "(int * x, value * v){ *x = Int_val(*v); }";
      init = Fmt.any "(int * x){ }";
      init_expr = Fmt.nop;
    }
