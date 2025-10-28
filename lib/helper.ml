open Type

(** Native integer, the last bit is lost during translation *)
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
    extra_defs = Fmt.nop;
  }

let ptr_ref ty =
  {
    name = "ref_" ^ ty.name;
    descr = "ref on " ^ ty.descr;
    deps = [ ty ];
    cty = (fun fmt () -> Fmt.pf fmt "%a *" cty ty);
    mlty = (fun fmt () -> Fmt.pf fmt "= %a" mlty ty);
    mlname = "camlid_ml_ref_" ^ ty.mlname;
    c2ml =
      (fun fmt () ->
        Fmt.pf fmt "(value * v, %a ** x){ %a(v,*x); }" cty ty c2ml ty);
    ml2c =
      (fun fmt () ->
        Fmt.pf fmt "(%a ** x, value * v){ %a(*x,v); }" cty ty ml2c ty);
    init = (fun fmt () -> Fmt.pf fmt "(%a ** x){ }" cty ty);
    init_expr =
      (fun fmt () ->
        Fmt.pf fmt "&(((struct { %a a; }) { %a }).a)" cty ty ty.init_expr ());
    extra_defs = Fmt.nop;
  }

(** Encapsulate a c type into an abstract ml type *)
let abstract ?get ?set ?internal ~ml ~c () =
  let name = "abstract_" ^ ml in
  let iname = if Option.is_none internal then name else "custom_intern_" ^ ml in

  {
    name;
    descr = Printf.sprintf "abstract tag for type \"%s\"" c;
    deps = [];
    cty = (fun fmt () -> Fmt.string fmt c);
    mlty = Fmt.nop;
    mlname = ml;
    ml2c =
      (fun fmt () ->
        match get with
        | None ->
            Fmt.pf fmt "(%a * x,value * v){ *x = *((%a *) Bp_val(*v)); }"
              cty_of_name name cty_of_name iname
        | Some f ->
            Fmt.pf fmt "(%a * x,value * v){ %s(x,((%a *) Bp_val(*v))); }"
              cty_of_name name f cty_of_name iname);
    c2ml =
      (fun fmt () ->
        Fmt.pf fmt
          "(value * v,%a * x){@\n\
           *v = caml_alloc((sizeof(%a) + sizeof(value) - 1) / sizeof(value), \
           Abstract_tag);@\n"
          cty_of_name name cty_of_name iname;
        match set with
        | None -> Fmt.pf fmt "*((%a *) Bp_val(*v)) = *x; }" cty_of_name iname
        | Some f ->
            Fmt.pf fmt "%s(((%a *) Bp_val(*v)),x); }" f cty_of_name iname);
    init = (fun fmt () -> Fmt.pf fmt "(%a * x){ }" cty_of_name name);
    init_expr = (fun fmt () -> Fmt.pf fmt "((%a) { })" cty_of_name name);
    extra_defs =
      (fun fmt () ->
        (match internal with
        | None -> ()
        | Some c -> Fmt.pf fmt "typedef %s %a;@\n" c cty_of_name iname);
        (match get with
        | None -> ()
        | Some f ->
            Fmt.pf fmt "void %s(%a *, %a *);@\n" f cty_of_name name cty_of_name
              iname);
        match set with
        | None -> ()
        | Some f ->
            Fmt.pf fmt "void %s(%a *, %a *);@\n" f cty_of_name iname cty_of_name
              name);
  }

(** Encapsulate a c type into an custom ml type *)
let custom ?finalize ?initialize ?hash ?compare ?get ?set ?internal ~ml ~c () =
  let name = "custom_" ^ ml in
  let iname = if Option.is_none internal then name else "custom_intern_" ^ ml in
  {
    name;
    descr = Printf.sprintf "abstract tag for type \"%s\"" c;
    deps = [];
    cty = (fun fmt () -> Fmt.string fmt c);
    mlty = Fmt.nop;
    mlname = ml;
    ml2c =
      (fun fmt () ->
        match get with
        | None ->
            Fmt.pf fmt
              "(%a * x,value * v){ *x = *((%a *)  Data_custom_val(*v)); }"
              cty_of_name name cty_of_name iname
        | Some f ->
            Fmt.pf fmt
              "(%a * x,value * v){ %s(x,((%a *)  Data_custom_val(*v))); }"
              cty_of_name name f cty_of_name iname);
    c2ml =
      (fun fmt () ->
        Fmt.pf fmt
          "(value * v,%a * x){@\n\
           *v = caml_alloc_custom(&camlid_cops_%s,sizeof(%a), 0, 1);@\n"
          cty_of_name name ml cty_of_name iname;
        match set with
        | None ->
            Fmt.pf fmt "*((%a *) Data_custom_val(*v)) = *x; }" cty_of_name iname
        | Some f ->
            Fmt.pf fmt "%s(((%a *) Data_custom_val(*v)),x); }" f cty_of_name
              iname);
    init =
      (fun fmt () ->
        let pp_init fmt f = Fmt.pf fmt "%s(x)" f in
        Fmt.pf fmt "(%a * x){ %a}" cty_of_name name
          Fmt.(option pp_init)
          initialize);
    init_expr = (fun fmt () -> Fmt.pf fmt "((%a) { })" cty_of_name name);
    extra_defs =
      (fun fmt () ->
        (match internal with
        | None -> ()
        | Some c -> Fmt.pf fmt "typedef %s %a;@\n" c cty_of_name iname);
        (match get with
        | None -> ()
        | Some f ->
            Fmt.pf fmt "void %s(%a *, %a *);@\n" f cty_of_name name cty_of_name
              iname);
        (match set with
        | None -> ()
        | Some f ->
            Fmt.pf fmt "void %s(%a *, %a *);@\n" f cty_of_name iname cty_of_name
              name);
        (match initialize with
        | None -> ()
        | Some f -> Fmt.pf fmt "void %s(%a *);@\n" f cty_of_name name);
        (match finalize with
        | None -> ()
        | Some f ->
            Fmt.pf fmt
              "void %s(%a *);@\n\
               static void camlid_ops_finalize_%s(value v)@\n\
               {@\n\
               %s((%a *) Data_custom_val(v));@\n\
               }@\n"
              f cty_of_name iname ml f cty_of_name iname);
        (match compare with
        | None -> ()
        | Some f ->
            Fmt.pf fmt
              "int %s(%a *, %a *);@\n\
               static int camlid_ops_compare_%s(value v1, value v2)@\n\
               {@\n\
               return %s((%a *) Data_custom_val(v1), (%a *) @\n\
               Data_custom_val(v2));@\n\
               }@\n"
              f cty_of_name iname cty_of_name iname ml f cty_of_name iname
              cty_of_name iname);
        (match hash with
        | None -> ()
        | Some f ->
            Fmt.pf fmt
              "long %s(%a *);\n\
               static long camlid_ops_hash_%s(value v)\n\
               {\n\
               return %s((%a *) Data_custom_val(v));\n\
               }\n"
              f cty_of_name iname ml f cty_of_name iname);
        let pp_op op_name fmt = function
          | None -> Fmt.pf fmt "custom_%s_default" op_name
          | Some _ -> Fmt.pf fmt "camlid_ops_%s_%s" op_name ml
        in
        Fmt.pf fmt
          "struct custom_operations camlid_cops_%s = {\n\
           NULL,\n\
           %a,\n\
           %a,\n\
           %a,\n\
           custom_serialize_default,\n\
           custom_deserialize_default\n\
           };@\n"
          ml (pp_op "finalize") finalize (pp_op "compare") compare
          (pp_op "hash") hash);
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
