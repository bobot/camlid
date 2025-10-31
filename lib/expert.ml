open Expr
open Type

let typedef name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> dfp id "typedef %t %a;@." k pp_id id)

let mlalias name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> dfp ~kind:ML id "type %a = %t@." pp_id id k)

let mlabstract name =
  let id = ID.mk name in
  dfp ~kind:ML id "type %a@." pp_id id

let declare_existing ?(result = expr "void") f params =
  let id = ID.mk ~keep_name:true f in
  let pp_ty fmt (var : var) = var.ty.expr fmt () in
  dfp ~params id "@[<hv 2>@[%a %s(@]%a@[);@]@]@." result.expr () f
    Fmt.(list ~sep:(any ",@ ") pp_ty)
    params

let wrap_typedef ?c2ml ?ml2c ?init ?free ty =
  {
    ty with
    c2ml = Option.value ~default:ty.c2ml c2ml;
    ml2c = Option.value ~default:ty.ml2c ml2c;
    init = Option.value ~default:ty.init init;
    free = Option.value ~default:ty.free free;
  }

(** Native integer, the last bit is lost during translation *)
let builtin_mltypes ~ml_type ~c_type ~c2ml ~ml2c =
  let cty = typedef ml_type "%s" c_type in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "int";
    cty;
    mlty = mlalias ml_type "%s" ml_type;
    mlname = None;
    c2ml = code "c2ml" "*%a = %s(*%a);" pp_var v c2ml pp_var c;
    ml2c = code "ml2c" "*%a = %s(*%a);" pp_var c ml2c pp_var v;
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_code cty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let int : typedef =
  builtin_mltypes ~ml_type:"int" ~c_type:"intnat" ~c2ml:"Val_long"
    ~ml2c:"Long_val"

let int_trunc : typedef =
  builtin_mltypes ~ml_type:"int" ~c_type:"int" ~c2ml:"Val_int" ~ml2c:"Int_val"

let double : typedef =
  builtin_mltypes ~ml_type:"float" ~c_type:"double" ~c2ml:"caml_copy_double"
    ~ml2c:"Double_val"

let int32 : typedef =
  builtin_mltypes ~ml_type:"int32" ~c_type:"int32_t" ~c2ml:"caml_copy_int32"
    ~ml2c:"Int32_val"

let int64 : typedef =
  builtin_mltypes ~ml_type:"int64" ~c_type:"int64_t" ~c2ml:"caml_copy_int64"
    ~ml2c:"Int64_val"

let nativeint : typedef =
  builtin_mltypes ~ml_type:"nativeint" ~c_type:"intnat"
    ~c2ml:"caml_copy_nativeint" ~ml2c:"Nativeint_val"

let ptr_ref (ty : typedef) =
  let cty = typedef "ref" "%a *" pp_code ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "ref on " ^ ty.descr;
    cty;
    mlty = mlalias "ptr_ref" "%a" pp_code ty.mlty;
    mlname = None;
    c2ml =
      code "c2ml" "%a;"
        (c2ml ~v:(expr "%a" pp_var v) ~c:(expr "*%a" pp_var c) ())
        ty;
    ml2c =
      code "ml2c" "%a;"
        (ml2c ~v:(expr "%a" pp_var v) ~c:(expr "*%a" pp_var c) ())
        ty;
    init = code "init" "%a;" (init ~c:(expr "*%a" pp_var c) ()) ty;
    init_expr =
      expr "&(((struct { %a a; }) { %a }).a)" pp_code ty.cty init_expr ty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let array ~len (ty : typedef) =
  let cty = typedef "array" "%a*" pp_code ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "array_length on " ^ ty.descr;
    cty;
    mlty = mlalias "array" "%a array" pp_code ty.mlty;
    mlname = None;
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "*%a=caml_alloc(%a,0);@," pp_var v pp_var len;
          fmt
            "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
             ){@]@,\
             %a;@,\
             Store_field(*%a,cid_i,cid_temp);@,\
             }@]@,"
            pp_var len
            (c2ml ~v:(expr "&cid_temp") ~c:(expr "&((*%a)[cid_i])" pp_var c) ())
            ty pp_var v;
          fmt "CAMLreturn0;");
    ml2c =
      codef "ml2c" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "@[*%a = malloc(sizeof(%a)*%a);@]@," pp_var c pp_code ty.cty
            pp_var len;
          fmt
            "@[<hv 2>@[<hv 2>for(@,\
             size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
             ){@]@,\
             cid_temp=Field(*%a,cid_i);@,\
             %a;@,\
             }@]@,"
            pp_var len pp_var v
            (ml2c ~v:(expr "&cid_temp") ~c:(expr "&((*%a)[cid_i])" pp_var c) ())
            ty;
          fmt "CAMLreturn0;");
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_code cty;
    free = code "free" "free(*%a);" pp_var c;
    v;
    c;
  }

let array_length (ty : typedef) =
  let sstruct =
    let id = ID.mk "array_s" in
    toplevel id "struct %a { %a* t; size_t len; };@." pp_id id pp_code ty.cty
  in
  let cty = typedef "array" "struct %a" pp_code sstruct in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "array_length on " ^ ty.descr;
    cty;
    mlty = mlalias "array" "%a array" pp_code ty.mlty;
    mlname = None;
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "*%a=caml_alloc(%a->len,0);@," pp_var v pp_var c;
          fmt
            "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
             ){@]@,\
             %a;@,\
             Store_field(*%a,cid_i,cid_temp);@,\
             }@]@,"
            pp_var c
            (c2ml ~v:(expr "&cid_temp") ~c:(expr "&%a->t[cid_i]" pp_var c) ())
            ty pp_var v;
          fmt "CAMLreturn0;");
    ml2c =
      codef "ml2c" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "@[%a->len = Wosize_val(*%a);@]@," pp_var c pp_var v;
          fmt "@[%a->t = malloc(sizeof(%a)*%a->len);@]@," pp_var c pp_code
            ty.cty pp_var c;
          fmt
            "@[<hv 2>@[<hv 2>for(@,\
             size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
             ){@]@,\
             cid_temp=Field(*%a,cid_i);@,\
             %a;@,\
             }@]@,"
            pp_var c pp_var v
            (ml2c ~v:(expr "&cid_temp") ~c:(expr "&%a->t[cid_i]" pp_var c) ())
            ty;
          fmt "CAMLreturn0;");
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_code cty;
    free = code "free" "free(%a->t);" pp_var c;
    v;
    c;
  }

let array_ptr_of_array_length ty array_length =
  let cty = typedef "array" "%a*" pp_code ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "array on " ^ ty.descr;
    cty;
    mlty = mlabstract "should_not_appear";
    mlname = None;
    c2ml = code "should_not_appear" "";
    ml2c = code "should not appear" "";
    init = code "init" "*%a = %a->t;" pp_var c pp_var array_length.c;
    init_expr = expr "0";
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let length_ptr_of_array_length ty array_length =
  let cty = typedef "length_array" "size_t*" in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "array on " ^ ty.descr;
    cty;
    mlty = mlabstract "should_not_appear";
    mlname = None;
    c2ml = code "should_not_appear" "";
    ml2c = code "should not appear" "";
    init = code "init" "*%a = &%a->len;" pp_var c pp_var array_length.c;
    init_expr = expr "0";
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let length_of_array_length ty array_length =
  let cty = typedef "length_array" "size_t" in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "array on " ^ ty.descr;
    cty;
    mlty = mlabstract "should_not_appear";
    mlname = None;
    c2ml = code "should_not_appear" "";
    ml2c = code "should not appear" "";
    init = code "init" "*%a = %a->len;" pp_var c pp_var array_length.c;
    init_expr = expr "0";
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

type get = {
  get : code;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

type set = {
  set : code;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

(** Encapsulate a c type into an abstract ml type *)
let abstract ?get ?set ~icty ~descr ~ml ~cty () =
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code (cty : code)) in
  {
    descr;
    cty;
    v;
    c;
    mlty = mlabstract ml;
    mlname = Some ml;
    ml2c =
      (match get with
      | None ->
          code "ml2c" "*%a = *((%a *) Bp_val(*%a));" pp_var c pp_code icty
            pp_var v
      | Some f ->
          code "ml2c" "%a;" pp_call
            ( f.get,
              [
                (f.c, expr "%a" pp_var c);
                (f.i, expr "((%a *) Bp_val(*%a))" pp_code icty pp_var v);
              ] ));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt
            "@[*%a = caml_alloc((sizeof(%a) + sizeof(value) - 1) / \
             sizeof(value), Abstract_tag);@]@,"
            pp_var v pp_code icty;
          match set with
          | None ->
              fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_code icty pp_var v pp_var c
          | Some f ->
              fmt "%a;" pp_call
                ( f.set,
                  [
                    (f.c, expr "%a" pp_var c);
                    (f.i, expr "((%a *) Bp_val(*%a))" pp_code icty pp_var v);
                  ] ));
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_code cty;
    free = code ~ovars:[ c ] "free" "";
  }

type finalize = { finalize : code; i : var }
type finalize_op = { finalize_op : code; v : var }
type initialize = { initialize : code; c : var }
type hash = { hash : code; i : var }
type hash_op = { hash_op : code; v : var }
type compare = { compare : code; i1 : var; i2 : var }
type compare_op = { compare_op : code; v1 : var; v2 : var }

(** Encapsulate a c type into an custom ml type *)
let custom ?finalize ?initialize ?hash ?compare ?get ?set ?internal ~ml ~c () =
  let descr = Printf.sprintf "abstract tag for type \"%s\"" c in
  let cty = typedef "custom" "%s" c in
  let icty =
    match internal with None -> cty | Some c -> typedef "custom_intern" "%s" c
  in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  let get =
    Option.map
      (fun get ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { get = declare_existing get [ c; i ]; c; i })
      get
  in
  let set =
    Option.map
      (fun set ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { set = declare_existing set [ i; c ]; c; i })
      set
  in
  let finalize =
    Option.map
      (fun finalize ->
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { finalize = declare_existing finalize [ i ]; i })
      finalize
  in
  let data_custom_val icty v =
    expr "(%a *) Data_custom_val(%a)" pp_code icty pp_var v
  in
  let data_custom_val' icty v =
    expr "(%a *) Data_custom_val(*%a)" pp_code icty pp_var v
  in
  let finalize_op =
    Option.map
      (fun finalize ->
        let v = Var.mk "v" (expr "value") in
        {
          finalize_op =
            code "finalize_op" "%a;" pp_call
              (finalize.finalize, [ (finalize.i, data_custom_val icty v) ]);
          v;
        })
      finalize
  in
  let hash =
    Option.map
      (fun hash ->
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { hash = declare_existing ~result:(expr "intnat") hash [ i ]; i })
      hash
  in
  let hash_op =
    Option.map
      (fun hash ->
        let v = Var.mk "v" (expr "value") in
        {
          hash_op =
            code ~ret:"intnat" "hash_op" "return %a;" pp_call
              (hash.hash, [ (hash.i, data_custom_val icty v) ]);
          v;
        })
      hash
  in
  let compare =
    Option.map
      (fun compare ->
        let i1 = Var.mk "c" (expr "%a *" pp_code icty) in
        let i2 = Var.mk "i" (expr "%a *" pp_code icty) in
        {
          compare = declare_existing ~result:(expr "int") compare [ i1; i2 ];
          i1;
          i2;
        })
      compare
  in
  let compare_op =
    Option.map
      (fun compare ->
        let v1 = Var.mk "v1" (expr "value") in
        let v2 = Var.mk "v2" (expr "value") in
        {
          compare_op =
            code ~ret:"int" "compare_op" "return %a;" pp_call
              ( compare.compare,
                [
                  (compare.i1, data_custom_val icty v1);
                  (compare.i2, data_custom_val icty v2);
                ] );
          v1;
          v2;
        })
      compare
  in
  let initialize =
    Option.map
      (fun finalize ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        { initialize = declare_existing finalize [ c ]; c })
      initialize
  in
  let custom_op =
    let id = ID.mk "cops" in
    let pp_op op_name get fmt = function
      | None -> Fmt.pf fmt "custom_%s_default" op_name
      | Some f -> Fmt.pf fmt "%a" pp_code (get f)
    in
    toplevel id
      "struct custom_operations %a = {\n\
       NULL,\n\
       %a,\n\
       %a,\n\
       %a,\n\
       custom_serialize_default,\n\
       custom_deserialize_default\n\
       };@."
      pp_id id
      (pp_op "finalize" (fun f -> f.finalize_op))
      finalize_op
      (pp_op "compare" (fun f -> f.compare_op))
      compare_op
      (pp_op "hash" (fun f -> f.hash_op))
      hash_op
  in
  {
    descr;
    cty;
    mlty = mlabstract ml;
    mlname = Some ml;
    ml2c =
      (match get with
      | None ->
          code "ml2c" "*%a = *(%a);" pp_var c (data_custom_val' icty v).expr ()
      | Some f ->
          code "ml2c" "@[%a;@]" pp_call
            ( f.get,
              [ (f.i, data_custom_val' icty v); (f.c, expr "%a" pp_var c) ] ));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@," pp_var v
            pp_code custom_op pp_code icty;
          match set with
          | None ->
              fmt "@[*(%a) = *%a;@]" (data_custom_val' icty v).expr () pp_var c
          | Some f ->
              fmt "@[%a;@]" pp_call
                ( f.set,
                  [ (f.i, data_custom_val' icty v); (f.c, expr "%a" pp_var c) ]
                ));
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, expr "%a" pp_var c) ])
       in
       code ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_code cty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let simple_param ?(binds = []) ?(input = false) ?(output = false)
    ?(used_in_call = true) pty pname =
  {
    input;
    output;
    used_in_call;
    pty;
    pc = Var.mk pname (expr "%a" pp_code pty.cty);
    binds;
  }

let input ?used_in_call ?binds = simple_param ?used_in_call ?binds ~input:true
let output ?used_in_call ?binds = simple_param ?used_in_call ?binds ~output:true

let inout ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:true ~output:true

let ignored ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:false ~output:false

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

type func = { fname : string; params : param list; result : result option }

let results f =
  let out_params = List.filter (fun p -> p.output) f.params in
  match f.result with
  | None -> out_params
  | Some result ->
      if result.routput then
        {
          input = false;
          output = false;
          used_in_call = false;
          pty = result.rty;
          pc = result.rc;
          binds = result.binds;
        }
        :: out_params
      else out_params

let return_var = Var.mk "ret" (expr "value")

let code_c_fun (f : func) =
  let inputs =
    List.filter_map
      (fun p ->
        if p.input then Some (p, Var.mk p.pc.name (expr "value")) else None)
      f.params
  in
  let results = results f in
  let used_in_calls = List.filter (fun p -> p.used_in_call) f.params in
  let vars_used_in_calls = List.map (fun p -> p.pc) used_in_calls in
  (* C function declaration *)
  let fid =
    declare_existing
      ?result:(Option.map (fun r -> expr "%a" pp_code r.rty.cty) f.result)
      f.fname vars_used_in_calls
  in
  (* local C variable declaration *)
  let tuple_var = Var.mk "tup" (expr "value[%i]" (List.length results)) in
  let id = ID.mk ("stub_" ^ f.fname) in
  fp ~kind:C ~params:(List.map snd inputs) id (fun { fmt } ->
      (* Formals *)
      let pp_formal fmt (_, pv) = Fmt.pf fmt "value %a" pp_var pv in
      fmt "@[<hv>@[<hv 2>@[extern value %a@](%a)@[{@]@," pp_id id
        Fmt.(list ~sep:comma pp_formal)
        inputs;
      (* Locals *)
      let pp_local fmt p =
        Fmt.pf fmt "@[%a %a = %a;@]@," pp_code p.pty.cty pp_var p.pc init_expr
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_local) f.params;
      (match f.result with
      | None -> ()
      | Some result ->
          fmt "@[%a %a;@]@," pp_code result.rty.cty pp_var result.rc);
      fmt "@[value %a;@]@," pp_var return_var;
      (match results with
      | [] | [ _ ] -> ()
      | l ->
          fmt "@[value %a;@]@," pp_var return_var;
          fmt "@[value %a[%i] = {%a};@]@," pp_var tuple_var (List.length l)
            Fmt.(list ~sep:comma (Fmt.any "Val_unit"))
            l);
      (* convert input variables *)
      let pp_conv_in fmt ((p : param), vc) =
        Fmt.pf fmt "@[%a;@]@,"
          (ml2c ~binds:p.binds ~v:(expr "&%a" pp_var vc)
             ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_conv_in) inputs;
      (* initialize variables that are not input *)
      let pp_init_out fmt p =
        if not p.input then
          Fmt.pf fmt "@[%a;@]@,"
            (init ~binds:p.binds ~c:(expr "&%a" pp_var p.pc) ())
            p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_init_out) f.params;
      (* function call *)
      let pp_result fmt = function
        | None -> ()
        | Some r -> Fmt.pf fmt "%a = " pp_var r.rc
      in
      fmt "@[%a%a;@]@," pp_result f.result pp_call
        (fid, List.map (fun v -> (v, expr "%a" pp_var v)) vars_used_in_calls);
      (* create return value *)
      let pp_conv_out fmt (p : param) =
        Fmt.pf fmt "@[%a;@]@,"
          (c2ml ~binds:p.binds
             ~v:(expr "&%a" pp_var return_var)
             ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      (match results with
      | [] -> fmt "@[%a = Val_unit;@]@," pp_var return_var
      | [ p ] ->
          (* convert uniq output *)
          fmt "%a" pp_conv_out p
      | l ->
          let len = List.length l in
          let li = List.mapi (fun i p -> (i, p)) l in
          fmt "@[Begin_roots_block(%a, %i)]@," pp_var tuple_var len;
          (* convert outputs *)
          let pp_conv_out fmt (i, (p : param)) =
            Fmt.pf fmt "@[%a;@]@,"
              (c2ml
                 ~v:(expr "&%a[%i]" pp_var tuple_var i)
                 ~c:(expr "&%a" pp_var p.pc) ~binds:p.binds ())
              p.pty
          in
          fmt "%a" Fmt.(list ~sep:Fmt.cut pp_conv_out) li;
          (* create output tuple *)
          fmt "@[%a = caml_alloc(%i,0);]@," pp_var return_var len;
          let pp_store fmt (i, _) =
            Fmt.pf fmt "@[Store_field(%a, %i, %a);]@," pp_var return_var i
              pp_var tuple_var
          in
          fmt "%a" Fmt.(list ~sep:Fmt.cut pp_store) li;
          fmt "@[End_roots()]@,");
      (* free allocated memory *)
      let pp_init_out fmt (p : param) =
        Fmt.pf fmt "@[%a;@]@,"
          (free ~binds:p.binds ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_init_out) f.params;
      (* return *)
      fmt "@[return %a;@]" pp_var return_var;
      fmt "@]@,@[};@]@]@.")

let print_ml_fun (f : func) =
  let code_c = code_c_fun f in
  let results = results f in
  let inputs = List.filter (fun p -> p.input) f.params in
  let pp_result fmt p = pp_code fmt p.pty.mlty in
  let pp_param fmt p = Fmt.pf fmt "%a -> " pp_code p.pty.mlty in
  expr "@[external %s: %a%a = \"%a\"@]" f.fname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "*") pp_result)
    results pp_code code_c

let func fname ?result ?ignored_result params =
  match (result, ignored_result) with
  | Some _, Some _ ->
      failwith "Camlid.Helper.func: can't set both result and ignored_result"
  | Some rty, None ->
      print_ml_fun
        {
          fname;
          params;
          result =
            Some
              {
                rty;
                routput = true;
                rc = Var.mk "res" (expr "%a" pp_code rty.cty);
                binds = [];
              };
        }
  | None, Some rty ->
      print_ml_fun
        {
          fname;
          params;
          result =
            Some
              {
                rty;
                routput = false;
                rc = Var.mk "res" (expr "%a" pp_code rty.cty);
                binds = [];
              };
        }
  | None, None -> print_ml_fun { fname; params; result = None }
