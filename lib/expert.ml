open Expr
open Type

let typedef name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> ddef ~kind:H id "typedef %t %a;@." k pp_id id)

let mlalias name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> ddef ~kind:ML id "type %a = %t@." pp_id id k)

let mlabstract name =
  let id = ID.mk name in
  ddef ~kind:ML id "type %a@." pp_id id

let declare_existing ?(result = expr "void") f params =
  let id = ID.mk ~keep_name:true f in
  let pp_ty fmt (var : var) = var.ty.expr fmt () in
  dfp ~params id "@[<hv 2>@[%a %s(@]%a@[);@]@]@." result.expr () f
    Fmt.(list ~sep:(any ",@ ") pp_ty)
    params

(** But don't declare *)
let existing f params =
  let id = ID.mk ~keep_name:true f in
  dfp ~params id ""

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
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    descr = "int";
    cty;
    mlty = mlalias ml_type "%s" ml_type;
    mlname = None;
    c2ml = code "c2ml" "*%a = %s(*%a);" pp_var v c2ml pp_var c;
    ml2c = code "ml2c" "*%a = %s(*%a);" pp_var c ml2c pp_var v;
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_def cty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let ptr_ref (ty : typedef) =
  let cty = typedef "ref" "%a *" pp_def ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    descr = "ref on " ^ ty.descr;
    cty;
    mlty = mlalias "ptr_ref" "%a" pp_def ty.mlty;
    mlname = None;
    c2ml = code "c2ml" "%a" (c2ml ~v:(e_var v) ~c:(expr "*%a" pp_var c) ()) ty;
    ml2c = code "ml2c" "%a" (ml2c ~v:(e_var v) ~c:(expr "*%a" pp_var c) ()) ty;
    init = code "init" "%a" (init ~c:(expr "*%a" pp_var c) ()) ty;
    init_expr =
      expr "&(((struct { %a a; }) { %a }).a)" pp_def ty.cty init_expr ty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let array ~len (ty : typedef) =
  let cty = typedef "array" "%a*" pp_def ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    descr = "array_length on " ^ ty.descr;
    cty;
    mlty = mlalias "array" "%a array" pp_def ty.mlty;
    mlname = None;
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "*%a=caml_alloc(%a,0);@," pp_var v pp_var len;
          fmt
            "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
             ){@]@,\
             %a@,\
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
          fmt "@[*%a = malloc(sizeof(%a)*%a);@]@," pp_var c pp_def ty.cty pp_var
            len;
          fmt
            "@[<hv 2>@[<hv 2>for(@,\
             size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
             ){@]@,\
             cid_temp=Field(*%a,cid_i);@,\
             %a@,\
             }@]@,"
            pp_var len pp_var v
            (ml2c ~v:(expr "&cid_temp") ~c:(expr "&((*%a)[cid_i])" pp_var c) ())
            ty;
          fmt "CAMLreturn0;");
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_def cty;
    free = code "free" "free(*%a);" pp_var c;
    v;
    c;
  }

let array_length (ty : typedef) =
  let sstruct =
    let id = ID.mk "array_s" in
    toplevel id "struct %a { %a* t; size_t len; };@." pp_id id pp_def ty.cty
  in
  let cty = typedef "array" "struct %a" pp_def sstruct in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    descr = "array_length on " ^ ty.descr;
    cty;
    mlty = mlalias "array" "%a array" pp_def ty.mlty;
    mlname = None;
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "CAMLparam0 ();@,";
          fmt "CAMLlocal1(cid_temp);@,";
          fmt "*%a=caml_alloc(%a->len,0);@," pp_var v pp_var c;
          fmt
            "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
             ){@]@,\
             %a@,\
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
          fmt "@[%a->t = malloc(sizeof(%a)*%a->len);@]@," pp_var c pp_def ty.cty
            pp_var c;
          fmt
            "@[<hv 2>@[<hv 2>for(@,\
             size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
             ){@]@,\
             cid_temp=Field(*%a,cid_i);@,\
             %a@,\
             }@]@,"
            pp_var c pp_var v
            (ml2c ~v:(expr "&cid_temp") ~c:(expr "&%a->t[cid_i]" pp_var c) ())
            ty;
          fmt "CAMLreturn0;");
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_def cty;
    free = code "free" "free(%a->t);" pp_var c;
    v;
    c;
  }

let array_ptr_of_array_length ty array_length =
  let cty = typedef "array" "%a*" pp_def ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
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
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
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
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
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

type initialize = { initialize : code; c : var }

(** Encapsulate a c type into an abstract ml type *)
let abstract ?initialize ?get ?set ~icty ~descr ~ml ~cty () =
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
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
          code "ml2c" "*%a = *((%a *) Bp_val(*%a));" pp_var c pp_def icty pp_var
            v
      | Some f ->
          code "ml2c" "%a" pp_calli
            ( f.get,
              [
                (f.c, e_var c);
                (f.i, expr "((%a *) Bp_val(*%a))" pp_def icty pp_var v);
              ] ));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt
            "@[*%a = caml_alloc((sizeof(%a) + sizeof(value) - 1) / \
             sizeof(value), Abstract_tag);@]@,"
            pp_var v pp_def icty;
          match set with
          | None ->
              fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_def icty pp_var v pp_var c
          | Some f ->
              fmt "%a" pp_calli
                ( f.set,
                  [
                    (f.c, e_var c);
                    (f.i, expr "((%a *) Bp_val(*%a))" pp_def icty pp_var v);
                  ] ));
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, e_var c) ])
       in
       code ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_def cty;
    free = code ~ovars:[ c ] "free" "";
  }

type finalize = { finalize : code; i : var }
type finalize_op = { finalize_op : code; v : var }
type hash = { hash : code; i : var }
type hash_op = { hash_op : code; v : var }
type compare = { compare : code; i1 : var; i2 : var }
type compare_op = { compare_op : code; v1 : var; v2 : var }

(** Encapsulate a c type into an custom ml type *)
let custom ?initialize ?finalize ?finalize_ptr ?hash ?compare ?get ?set ~ml
    ~icty ~cty () =
  let descr = Printf.sprintf "abstract tag for type \"%s\"" ml in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let data_custom_val icty v =
    expr "(%a *) Data_custom_val(%a)" pp_def icty pp_var v
  in
  let data_custom_val' icty v =
    expr "(%a *) Data_custom_val(*%a)" pp_def icty pp_var v
  in
  let finalize_op =
    match (finalize, finalize_ptr) with
    | None, None -> None
    | Some finalize, None ->
        let v = Var.mk "v" (expr "value") in
        Some
          {
            finalize_op =
              code "finalize_op" "%a" pp_calli
                (finalize.finalize, [ (finalize.i, data_custom_val icty v) ]);
            v;
          }
    | None, Some finalize ->
        let v = Var.mk "v" (expr "value") in
        Some
          {
            finalize_op =
              code "finalize_op" "%a" pp_calli
                ( finalize.finalize,
                  [ (finalize.i, expr "*%a" (data_custom_val icty v).expr ()) ]
                );
            v;
          }
    | Some _, Some _ -> Fmt.failwith "finalize and finalize_ptr given for %s" ml
  in
  let hash_op =
    Option.map
      (fun hash ->
        let v = Var.mk "v" (expr "value") in
        {
          hash_op =
            code ~ret:(expr "intptr_t") "hash_op" "return %a;" pp_call
              (hash.hash, [ (hash.i, data_custom_val icty v) ]);
          v;
        })
      hash
  in
  let compare_op =
    Option.map
      (fun compare ->
        let v1 = Var.mk "v1" (expr "value") in
        let v2 = Var.mk "v2" (expr "value") in
        {
          compare_op =
            code ~ret:(expr "int") "compare_op" "return %a;" pp_call
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
  let custom_op =
    let id = ID.mk "cops" in
    let pp_op op_name get fmt = function
      | None -> Fmt.pf fmt "custom_%s_default" op_name
      | Some f -> Fmt.pf fmt "%a" pp_def (def_of_code @@ get f)
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
            (f.get, [ (f.i, data_custom_val' icty v); (f.c, e_var c) ]));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@," pp_var v
            pp_def custom_op pp_def icty;
          match set with
          | None ->
              fmt "@[*(%a) = *%a;@]" (data_custom_val' icty v).expr () pp_var c
          | Some f ->
              fmt "@[%a;@]" pp_call
                (f.set, [ (f.i, data_custom_val' icty v); (f.c, e_var c) ]));
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, e_var c) ])
       in
       code ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_def cty;
    free = code ~ovars:[ c ] "free" "";
    v;
    c;
  }

let mk_get ~icty ~cty get =
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let i = Var.mk "i" (expr "%a *" pp_def icty) in
  { get = declare_existing get [ c; i ]; c; i }

let mk_set ~icty ~cty set =
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let i = Var.mk "i" (expr "%a *" pp_def icty) in
  { set = declare_existing set [ i; c ]; c; i }

let mk_finalize ~icty finalize =
  let i = Var.mk "i" (expr "%a *" pp_def icty) in
  { finalize = declare_existing finalize [ i ]; i }

let mk_finalize_ptr ~icty finalize =
  let i = Var.mk "i" (e_def icty) in
  { finalize = declare_existing finalize [ i ]; i }

let mk_hash ~icty hash =
  let i = Var.mk "i" (expr "%a *" pp_def icty) in
  { hash = declare_existing ~result:(expr "intptr_t") hash [ i ]; i }

let mk_compare ~icty compare =
  let i1 = Var.mk "c" (expr "%a *" pp_def icty) in
  let i2 = Var.mk "i" (expr "%a *" pp_def icty) in
  { compare = declare_existing ~result:(expr "int") compare [ i1; i2 ]; i1; i2 }

let mk_initialize ~cty initialize =
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  { initialize = declare_existing initialize [ c ]; c }

let simple_param ?(binds = []) ?(input = false) ?(output = false)
    ?(used_in_call = true) ?(name = "p") pty =
  { input; output; used_in_call; pty; pc = Var.mk name (e_def pty.cty); binds }

let input ?used_in_call ?binds = simple_param ?used_in_call ?binds ~input:true
let output ?used_in_call ?binds = simple_param ?used_in_call ?binds ~output:true

let inout ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:true ~output:true

let ignored ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:false ~output:false

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

type func = {
  fid : code;
  mlname : string;
  params : param list;
  result : result option;
}

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
  (* local C variable declaration *)
  let tuple_var = Var.mk "tup" (expr "value[%i]" (List.length results)) in
  let id = ID.mk ("stub_" ^ (def_of_def (def_of_code f.fid)).id.name) in
  fp ~kind:C ~params:(List.map snd inputs) id (fun { fmt } ->
      (* Formals *)
      let pp_formal fmt (_, pv) = Fmt.pf fmt "value %a" pp_var pv in
      fmt "@[<hv>@[<hv 2>@[extern value %a@](%a)@[{@]@," pp_id id
        Fmt.(list ~sep:comma pp_formal)
        inputs;
      (* Locals *)
      let pp_local fmt p =
        Fmt.pf fmt "@[%a %a = %a;@]@," pp_def p.pty.cty pp_var p.pc init_expr
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_local) f.params;
      (match f.result with
      | None -> ()
      | Some result -> fmt "@[%a %a;@]@," pp_def result.rty.cty pp_var result.rc);
      fmt "@[value %a;@]@," pp_var return_var;
      (match results with
      | [] | [ _ ] -> ()
      | l ->
          fmt "@[value %a[%i] = {%a};@]@," pp_var tuple_var (List.length l)
            Fmt.(list ~sep:comma (Fmt.any "Val_unit"))
            l);
      (* convert input variables *)
      let pp_conv_in fmt ((p : param), vc) =
        Fmt.pf fmt "@[%a@]@,"
          (ml2c ~binds:p.binds ~v:(expr "&%a" pp_var vc)
             ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_conv_in) inputs;
      (* initialize variables that are not input *)
      let pp_init_out fmt p =
        if not p.input then
          Fmt.pf fmt "@[%a@]@,"
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
        (f.fid, List.map (fun v -> (v, e_var v)) vars_used_in_calls);
      (* create return value *)
      let pp_conv_out fmt (p : param) =
        Fmt.pf fmt "@[%a@]@,"
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
          fmt "@[<hv 2>@[Begin_roots_block(%a, %i)@]@," pp_var tuple_var len;
          (* convert outputs *)
          let pp_conv_out fmt (i, (p : param)) =
            Fmt.pf fmt "@[%a@]"
              (c2ml
                 ~v:(expr "&%a[%i]" pp_var tuple_var i)
                 ~c:(expr "&%a" pp_var p.pc) ~binds:p.binds ())
              p.pty
          in
          fmt "%a@," Fmt.(list ~sep:Fmt.cut pp_conv_out) li;
          (* create output tuple *)
          fmt "@[%a = caml_alloc(%i,0);@]@," pp_var return_var len;
          let pp_store fmt (i, _) =
            Fmt.pf fmt "@[Store_field(%a, %i, %a[%i]);@]" pp_var return_var i
              pp_var tuple_var i
          in
          fmt "%a@]@," Fmt.(list ~sep:Fmt.cut pp_store) li;
          fmt "@[End_roots()@]@,");
      (* free allocated memory *)
      let pp_init_out fmt (p : param) =
        Fmt.pf fmt "@[%a@]@,"
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
  let pp_result fmt p = pp_def fmt p.pty.mlty in
  let pp_param fmt p = Fmt.pf fmt "@[%a ->@]@ " pp_def p.pty.mlty in
  expr "@[<hv 2>external %s:@ %a@[<hv>%a@]@ = \"%a\"@]" f.mlname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "@ *@ ") pp_result)
    results pp_def (def_of_code code_c)

let declare_struct name fields =
  let id = ID.mk name in
  let pp_field fmt (name, ty) = Fmt.pf fmt "%a %s;" ty.expr () name in
  toplevel ~kind:H id "@[<hv 2>struct %a {@,%a@,};@]@." pp_id id
    Fmt.(list ~sep:cut pp_field)
    fields

let if_ ?else_ cond ~then_ =
  match else_ with
  | Some else_ ->
      expr "@[<hv 2>if(%a){@ %a@ } else {@ %a@ };@]" cond.expr () then_.expr ()
        else_.expr ()
  | None -> expr "@[<hv 2>if(%a){@ %a@ };@]" cond.expr () then_.expr ()

let seq l = expr "%a" Fmt.(list ~sep:cut (fun fmt e -> e.expr fmt ())) l

type convert = { convert : code; src : Var.t; dst : Var.t }

let mk_converter ~src ~dst name params =
  let dst = Expr.Var.mk "dst" (expr "%a *" pp_def dst.cty) in
  let src = Expr.Var.mk "src" (expr "%a *" pp_def src.cty) in
  let params = params ~src ~dst in
  { dst; src; convert = mk ~params (ID.mk ~keep_name:true name) Fmt.nop }

let convert ?a_to_b ?b_to_a ~(a : typedef) ~(b : typedef) () =
  {
    descr = "int";
    cty = b.cty;
    mlty = a.mlty;
    mlname = a.mlname;
    c2ml =
      (match b_to_a with
      | None -> code "no_b_to_a_given" ""
      | Some (b_to_a : convert) ->
          code "c2ml" "%a tmp; %a;@ %a" pp_def a.cty pp_call
            ( b_to_a.convert,
              [ (b_to_a.src, e_var b.c); (b_to_a.dst, expr "&tmp") ] )
            (c2ml ~v:(e_var a.v) ~c:(expr "&tmp") ())
            a);
    ml2c =
      (match a_to_b with
      | None -> code "no_a_to_b_given" ""
      | Some (a_to_b : convert) ->
          code "c2ml" "%a tmp; %a@ %a;" pp_def a.cty
            (ml2c ~v:(e_var a.v) ~c:(expr "&tmp") ())
            a pp_call
            ( a_to_b.convert,
              [ (a_to_b.src, e_var b.c); (a_to_b.dst, expr "&tmp") ] ));
    init = b.init;
    init_expr = b.init_expr;
    free = b.free;
    v = a.v;
    c = b.c;
  }

module AlgData = struct
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * typedef) list

  type constr = {
    name : string;
    tag : ID.t;
    smart_constructor : code;
    kind : kind;
  }

  type t = {
    ty : typedef;
    constrs : constr list;
    dst_smart_constructors : Expr.var;
  }

  let algdata ml_type l : t =
    let _, l =
      List.fold_left_map
        (fun (id_cst, id_non_cst) (name, fields) ->
          let id = ID.mk (Printf.sprintf "%s_%s" ml_type name) in
          if List.is_empty fields then
            ((id_cst + 1, id_non_cst), (name, id, KConst id_cst))
          else
            let fields =
              List.map
                (fun (n, ty) -> (n, Var.mk n (expr "%a*" pp_def ty.cty), ty))
                fields
            in
            ( (id_cst, id_non_cst + 1),
              (name, id, KNonConst (id_non_cst, fields)) ))
        (0, 0) l
    in
    let cty : defined =
      let pp_field fmt (var, _, ty) = Fmt.pf fmt "%a %s;" pp_def ty.cty var in
      let pp_constr fmt (name, _, fields) =
        match fields with
        | KConst _ -> ()
        | KNonConst (_, fields) ->
            Fmt.pf fmt "@[<hv 2>struct {@ %a@ } %s;@]"
              Fmt.(list ~sep:sp pp_field)
              fields name
      in
      let pp_enum fmt (_, id, _) = Fmt.pf fmt "%a" pp_id id in
      let id = ID.mk ml_type in
      toplevel ~kind:H id
        "@[<hv 2>@[typedef struct {@]@ @[<hv 2>enum {@ %a} tag;@]@ @[<hv \
         2>union {@ %a} u;@] @[}@] %a;@]@,"
        Fmt.(list ~sep:comma pp_enum)
        l
        Fmt.(list ~sep:sp pp_constr)
        l pp_id id
    in
    let mlty =
      let pp_field fmt (_, _, ty) = Fmt.pf fmt "%a" pp_def ty.mlty in
      let pp_constr fmt (name, _, fields) =
        match fields with
        | KConst _ -> Fmt.pf fmt "| %s" name
        | KNonConst (_, fields) ->
            Fmt.pf fmt "| %s of @[<hv>%a@]" name
              Fmt.(list ~sep:(any "*") pp_field)
              fields
      in
      let id = ID.mk ~keep_name:true ml_type in
      toplevel ~kind:ML id "@[<hv 2>type %a =@ %a@]@," pp_id id
        Fmt.(list ~sep:sp pp_constr)
        l
    in
    let v = Var.mk "v" (expr "value *") in
    let c = Var.mk "c" (expr "%a *" pp_def cty) in
    let c2ml =
      let pp_case fmt (name, id, fields) =
        Fmt.pf fmt "@[<hv 2>case %a: /* %s */@ " pp_id id name;
        (match fields with
        | KConst nc -> Fmt.pf fmt "*%a = Val_int(%i);@ " pp_var v nc
        | KNonConst (nc, fields) ->
            let nb_fields = List.length fields in
            Fmt.pf fmt "*%a=caml_alloc(%i,%i);@ " pp_var v nb_fields nc;
            let fields = List.mapi (fun i x -> (i, x)) fields in
            let pp_field fmt (i, (fname, _, ty)) =
              Fmt.pf fmt "%a@,"
                (c2ml ~v:(expr "&tmp")
                   ~c:(expr "&%a->u.%s.%s" pp_var c name fname)
                   ())
                ty;
              Fmt.pf fmt "Store_field(*%a,%i,tmp);@ " pp_var v i
            in
            Fmt.(list ~sep:sp pp_field) fmt fields);
        Fmt.pf fmt "break;@]"
      in
      code ~ovars:[ v; c ] "c2ml"
        "CAMLparam0();@ CAMLlocal1(tmp);@ switch(%a->tag){@,%a};@ CAMLreturn0;"
        pp_var c
        Fmt.(list ~sep:sp pp_case)
        l
    in
    let ty =
      {
        descr = "int";
        cty;
        mlty;
        mlname = None;
        c2ml;
        ml2c = code ~ovars:[ v; c ] "not_yet_implemented" "";
        init = code ~ovars:[ c ] "init" "";
        init_expr = expr "((%a) { })" pp_def cty;
        free = code ~ovars:[ c ] "free" "";
        v;
        c;
      }
    in
    let dst_smart_constructors = Var.mk "dst" (expr "%a*" pp_def cty) in
    let smart_constructor (cname, id, fields) =
      let fun_name = Fmt.str "mk_%s_%s" ml_type cname in
      let dst = dst_smart_constructors in
      match fields with
      | KConst _ ->
          let pp_doc fmt () =
            Fmt.pf fmt
              "@[// @brief Fill [dst] with the constant case %s of %s@]@," cname
              ml_type;
            Fmt.pf fmt "@[// @param dst structure to fill@]"
          in
          Type.code ~kind:H ~params:[ dst ]
            ~doc:Fmt.(vbox pp_doc ++ cut)
            fun_name "%a->tag=%a;" pp_var dst pp_id id
      | KNonConst (_, fields) ->
          let params = List.map (fun (_, v, _) -> v) fields in
          let pp_doc fmt () =
            Fmt.pf fmt
              "@[// @brief Fill [dst] with the constructor %s of %s@]@," cname
              ml_type;
            Fmt.pf fmt "@[// @param dst structure to fill@]@,";
            List.iter
              (fun (fname, _, _) ->
                Fmt.pf fmt
                  "@[// @param %s reference is assigned to the constructor \
                   field@]@,"
                  fname)
              fields
          in
          codef ~kind:H fun_name ~params:(dst :: params)
            ~doc:Fmt.(vbox pp_doc ++ cut)
            (fun { fmt } ->
              fmt "%a->tag=%a;@," pp_var dst pp_id id;
              let pp_field fmt (fname, v, _) =
                Fmt.pf fmt "%a->u.%s.%s = *%a;" pp_var dst cname fname pp_var v
              in
              fmt "%a" Fmt.(list ~sep:sp pp_field) fields)
    in
    let constrs =
      List.map
        (fun (name, id, kind) ->
          {
            name;
            tag = id;
            kind;
            smart_constructor = smart_constructor (name, id, kind);
          })
        l
    in
    { ty; constrs; dst_smart_constructors }
end
