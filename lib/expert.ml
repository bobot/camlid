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

let get_boxing = function
  | Boxed { c2ml; ml2c } -> (ml2c, c2ml)
  | Unboxable { c2ml; ml2c; _ } -> (ml2c, c2ml)

(** Native integer, the last bit is lost during translation *)
let builtin_mltypes ~ml_type ~c_type ~c2ml ~ml2c =
  let cty = typedef ml_type "%s" c_type in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    cty;
    mlty = mlalias ml_type "%s" ml_type;
    mlname = None;
    conv =
      Boxed
        {
          c2ml = code "c2ml" "*%a = %s(*%a);" pp_var v c2ml pp_var c;
          ml2c = code "ml2c" "*%a = %s(*%a);" pp_var c ml2c pp_var v;
        };
    init = None;
    init_expr = expr "((%a) { })" pp_def cty;
    free = None;
    v;
    c;
  }

(* string null terminated, convert until the first null character *)
let string_nt ?(owned = true) () =
  let cty = typedef "string_nt" "char *" in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    cty;
    mlty = mlalias "string_nt" "string";
    mlname = None;
    conv =
      Boxed
        {
          c2ml = code "c2ml" "*%a = caml_copy_string(*%a);" pp_var v pp_var c;
          ml2c =
            codef "ml2c" (fun { fmt } ->
                fmt "size_t len=strlen(String_val(*%a))+1;@ " pp_var v;
                fmt "*%a=malloc(len);@ " pp_var c;
                fmt "memcpy(*%a,String_val(*%a),len);" pp_var c pp_var v);
        };
    init = None;
    init_expr = expr "((%a) { })" pp_def cty;
    free = (if owned then codeo "free" "free(*%a);" pp_var c else None);
    v;
    c;
  }

let string_fixed_length ?(init = true) ?(owned = true) len =
  let cty = typedef "string_fs" "char *" in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let malloc { fmt } = fmt "*%a=malloc(%a);@ " pp_var c pp_var len in
  {
    cty;
    mlty = mlalias "string_fs" "string";
    mlname = None;
    conv =
      Boxed
        {
          c2ml =
            codef "c2ml" (fun { fmt } ->
                fmt "*%a = caml_alloc_string(%a);" pp_var v pp_var len;
                fmt "memcpy(&Byte(*%a,0),*%a,%a);" pp_var v pp_var c pp_var len);
          ml2c =
            codef "ml2c" (fun { fmt } ->
                malloc { fmt };
                fmt "memcpy(*%a,String_val(*%a),%a);" pp_var c pp_var v pp_var
                  len);
        };
    init = (if init then codefo "init" malloc else None);
    init_expr = expr "((%a) { })" pp_def cty;
    free = (if owned then codeo "free" "free(*%a);" pp_var c else None);
    v;
    c;
  }

let string_length_struct =
  let id = ID.mk "string_s" in
  toplevel id "struct %a { char* t; size_t len; };@." pp_id id

let string_length ?(owned = true) () =
  let cty = typedef "string_length" "struct %a" pp_def string_length_struct in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    cty;
    mlty = mlalias "string_length" "string";
    mlname = None;
    conv =
      Boxed
        {
          c2ml =
            codef "c2ml" (fun { fmt } ->
                fmt "*%a = caml_alloc_string(%a->len);" pp_var v pp_var c;
                fmt "memcpy(&Byte(*%a,0),%a->t,%a->len);" pp_var v pp_var c
                  pp_var c);
          ml2c =
            codef "ml2c" (fun { fmt } ->
                fmt "%a->len=caml_string_length(%a);@ " pp_var c pp_var v;
                fmt "%a->t=malloc(%a->len);@ " pp_var c pp_var c;
                fmt "memcpy(%a->t,String_val(*%a),%a->len);" pp_var c pp_var v
                  pp_var c);
        };
    init = None;
    init_expr = expr "((%a) { })" pp_def cty;
    free = (if owned then codeo "free" "free(*%a);" pp_var c else None);
    v;
    c;
  }

let ptr_ref (ty : typedef) =
  let cty = typedef "ref" "%a *" pp_def ty.cty in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let conv =
    let add_addr name fid =
      code name "%a" pp_calli (fid, ty_binds ~c:(expr "*%a" pp_var c) ty)
    in
    match ty.conv with
    | Boxed { ml2c; c2ml } ->
        Boxed { ml2c = add_addr "ml2c" ml2c; c2ml = add_addr "c2ml" c2ml }
    | Unboxable { unbox_attribute; ucty; ml2u; u2ml; u2c; c2u; u; ml2c; c2ml }
      ->
        Unboxable
          {
            unbox_attribute;
            ucty;
            ml2u;
            u2ml;
            u2c = add_addr "u2c" u2c;
            c2u = add_addr "c2u" c2u;
            u;
            ml2c = add_addr "ml2c" ml2c;
            c2ml = add_addr "c2ml" c2ml;
          }
  in
  {
    cty;
    mlty = mlalias "ptr_ref" "%a" pp_def ty.mlty;
    mlname = None;
    conv;
    init =
      Option.map
        (fun init ->
          code "init" "%a" pp_calli (init, ty_binds ~c:(expr "*%a" pp_var c) ty))
        ty.init;
    init_expr =
      expr "&(((struct { %a a; }) { %a }).a)" pp_def ty.cty pp_expr ty.init_expr;
    free =
      Option.map
        (fun free ->
          code "free" "%a" pp_calli (free, ty_binds ~c:(expr "*%a" pp_var c) ty))
        ty.free;
    v = ty.v;
    c;
  }

let array ?(init = true) ?(owned = true) ~len (ty : typedef) =
  let cty = typedef "array" "%a*" pp_def ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let malloc { fmt } =
    fmt "@[*%a = malloc(sizeof(%a)*%a);@]@," pp_var c pp_def ty.cty pp_var len
  in
  {
    cty;
    mlty = mlalias "array" "%a array" pp_def ty.mlty;
    mlname = None;
    conv =
      (let ml2c, c2ml = get_boxing ty.conv in
       Boxed
         {
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
                   pp_var len pp_calli
                   ( c2ml,
                     ty_binds ~v:(expr "&cid_temp")
                       ~c:(expr "&((*%a)[cid_i])" pp_var c)
                       ty )
                   pp_var v;
                 fmt "CAMLreturn0;");
           ml2c =
             codef "ml2c" (fun { fmt } ->
                 fmt "CAMLparam0 ();@,";
                 fmt "CAMLlocal1(cid_temp);@,";
                 malloc { fmt };
                 fmt
                   "@[<hv 2>@[<hv 2>for(@,\
                    size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
                    ){@]@,\
                    cid_temp=Field(*%a,cid_i);@,\
                    %a@,\
                    }@]@,"
                   pp_var len pp_var v pp_calli
                   ( ml2c,
                     ty_binds ~v:(expr "&cid_temp")
                       ~c:(expr "&((*%a)[cid_i])" pp_var c)
                       ty );
                 fmt "CAMLreturn0;");
         });
    init = (if init then codefo "init" malloc else None);
    init_expr = expr "((%a) { })" pp_def cty;
    free = (if owned then codeo "free" "free(*%a);" pp_var c else None);
    v;
    c;
  }

let array_length ?(owned = true) (ty : typedef) =
  let sstruct =
    let id = ID.mk "array_s" in
    toplevel id "struct %a { %a* t; size_t len; };@." pp_id id pp_def ty.cty
  in
  let cty = typedef "array" "struct %a" pp_def sstruct in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    cty;
    mlty = mlalias "array" "%a array" pp_def ty.mlty;
    mlname = None;
    conv =
      (let ml2c, c2ml = get_boxing ty.conv in
       Boxed
         {
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
                   pp_var c pp_calli
                   ( c2ml,
                     ty_binds ~v:(expr "&cid_temp")
                       ~c:(expr "&%a->t[cid_i]" pp_var c)
                       ty )
                   pp_var v;
                 fmt "CAMLreturn0;");
           ml2c =
             codef "ml2c" (fun { fmt } ->
                 fmt "CAMLparam0 ();@,";
                 fmt "CAMLlocal1(cid_temp);@,";
                 fmt "@[%a->len = Wosize_val(*%a);@]@," pp_var c pp_var v;
                 fmt "@[%a->t = malloc(sizeof(%a)*%a->len);@]@," pp_var c pp_def
                   ty.cty pp_var c;
                 fmt
                   "@[<hv 2>@[<hv 2>for(@,\
                    size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
                    ){@]@,\
                    cid_temp=Field(*%a,cid_i);@,\
                    %a@,\
                    }@]@,"
                   pp_var c pp_var v pp_calli
                   ( ml2c,
                     ty_binds ~v:(expr "&cid_temp")
                       ~c:(expr "&%a->t[cid_i]" pp_var c)
                       ty );
                 fmt "CAMLreturn0;");
         });
    init = None;
    init_expr = expr "((%a) { })" pp_def cty;
    free = (if owned then codeo "free" "free(%a->t);" pp_var c else None);
    v;
    c;
  }

let map_param_in_call ?(name = "arg") map param =
  match param.pused_in_call with
  | None -> invalid_arg "Not used in call"
  | Some (pcall, e) ->
      let ty, arg = map pcall.ty e in
      let pcall2 = Var.mk name ty in
      { param with pused_in_call = Some (pcall2, arg) }

let use_ptr_of_param_in_call param =
  map_param_in_call ~name:"ptr"
    (fun ty e -> (expr "%a *" pp_expr ty, expr "&(%a)" pp_expr e))
    param

let use_new_param_only_in_call param =
  match param.pused_in_call with
  | None -> invalid_arg "Not used in call"
  | Some (pcall, e) ->
      let pcall2 = Var.mk "ptr" pcall.ty in
      {
        pused_in_call = Some (pcall2, e);
        pinput = PINone;
        poutput = PONone;
        pinit = None;
        pinit_expr = [];
        pfree = None;
      }

let array_of_array_length ty param =
  map_param_in_call ~name:"ptr"
    (fun _ e -> (expr "%a*" pp_def ty.cty, expr "%a.t" pp_expr e))
    param

let length_of_array_length param =
  map_param_in_call ~name:"ptr"
    (fun _ e -> (expr "size_t", expr "%a.len" pp_expr e))
    param

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
let abstract ?initialize ?get ?set ~icty ~ml ~cty () =
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    cty;
    v;
    c;
    mlty = mlabstract ml;
    mlname = Some ml;
    conv =
      Boxed
        {
          ml2c =
            (match get with
            | None ->
                code "ml2c" "*%a = *((%a *) Bp_val(*%a));" pp_var c pp_def icty
                  pp_var v
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
                    fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_def icty pp_var v
                      pp_var c
                | Some f ->
                    fmt "%a" pp_calli
                      ( f.set,
                        [
                          (f.c, e_var c);
                          (f.i, expr "((%a *) Bp_val(*%a))" pp_def icty pp_var v);
                        ] ));
        };
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, e_var c) ])
       in
       codeo ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_def cty;
    free = None;
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
    cty;
    mlty = mlabstract ml;
    mlname = Some ml;
    conv =
      Boxed
        {
          ml2c =
            (match get with
            | None ->
                code "ml2c" "*%a = *(%a);" pp_var c
                  (data_custom_val' icty v).expr ()
            | Some f ->
                code "ml2c" "@[%a;@]" pp_call
                  (f.get, [ (f.i, data_custom_val' icty v); (f.c, e_var c) ]));
          c2ml =
            codef "c2ml" (fun { fmt } ->
                fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@,"
                  pp_var v pp_def custom_op pp_def icty;
                match set with
                | None ->
                    fmt "@[*(%a) = *%a;@]" (data_custom_val' icty v).expr ()
                      pp_var c
                | Some f ->
                    fmt "@[%a;@]" pp_call
                      (f.set, [ (f.i, data_custom_val' icty v); (f.c, e_var c) ]));
        };
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, e_var c) ])
       in
       codeo ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_def cty;
    free = None;
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
  let pc = Var.mk name (e_def pty.cty) in
  let pc_call = Var.mk name (e_def pty.cty) in
  let pv = Var.mk name (expr "value") in
  let pv' = Var.mk (name ^ "_r") (expr "value") in
  let bind code =
    Expr.binds ((pty.c, e_addr pc) :: (pty.v, e_addr pv) :: binds) code
  in
  let bind' code =
    Expr.binds ((pty.c, e_addr pc) :: (pty.v, e_addr pv') :: binds) code
  in
  let pinput, pinit, poutput =
    match pty.conv with
    | Boxed { c2ml; ml2c } ->
        let pinput, pinit =
          if input then
            (PIBoxed { ml = pv; ml2c = bind ml2c; pmlty = pty.mlty }, None)
          else (PINone, Option.map bind pty.init)
        in
        let poutput =
          if output then
            POBoxed { ml = pv'; c2ml = bind' c2ml; pmlty = pty.mlty }
          else PONone
        in
        (pinput, pinit, poutput)
    | Unboxable
        { unbox_attribute; ucty; ml2u; u2ml; u2c; c2u; u; ml2c = _; c2ml } ->
        let pu = Var.mk name (e_def ucty) in
        let bind code =
          Expr.binds
            ((u, e_addr pu) :: (pty.c, e_addr pc) :: (pty.v, e_addr pv) :: binds)
            code
        in
        let bind' code =
          Expr.binds
            ((u, e_addr pu)
            :: (pty.c, e_addr pc)
            :: (pty.v, e_addr pv')
            :: binds)
            code
        in
        let pinput, pinit =
          if input then
            ( PIUnboxable
                {
                  unbox_attribute;
                  ml = pv;
                  u = pu;
                  u2c = bind u2c;
                  ml2u = bind ml2u;
                  pmlty = pty.mlty;
                },
              None )
          else (PINone, Option.map bind pty.init)
        in
        let poutput =
          if output then
            POUnboxable
              {
                unbox_attribute;
                ml = pv';
                u = pu;
                c2u = bind' c2u;
                u2ml = bind' u2ml;
                c2ml = bind' c2ml;
                pmlty = pty.mlty;
              }
          else PONone
        in
        (pinput, pinit, poutput)
  in
  let pused_in_call = if used_in_call then Some (pc_call, e_var pc) else None in
  let pinit_expr = [ (pc, Some pty.init_expr) ] in
  let pfree = Option.map bind pty.free in
  ({ pinput; pinit_expr; pinit; pused_in_call; pfree; poutput }, pc)

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

let add_result params result =
  match result with
  | Some ({ routput = POBoxed _ | POUnboxable _; _ } as result) ->
      {
        pinput = PINone;
        poutput = result.routput;
        pused_in_call = None;
        pfree = result.rfree;
        pinit = None;
        pinit_expr = [ (result.rc, None) ];
      }
      :: params
  | _ -> params

let return_var = Var.mk "ret" (expr "value")

type kind_of_result = UnitResult | MultipleValues | OneResult of var

let code_c_fun ~params ~result fid =
  let pp_scall proj { fmt } l =
    let pp fmt call = Fmt.pf fmt "@[%a@]@," pp_calli (call, []) in
    fmt "%a" Fmt.(list ~sep:nop pp) (List.filter_map proj l)
  in
  let params = add_result params result in
  let var_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml; _ } -> Some ml
    | PIUnboxable { u; _ } -> Some u
  in
  let boxed_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml; _ } -> Some ml
    | PIUnboxable _ -> None
  in
  let a2c_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml2c; _ } -> Some ml2c
    | PIUnboxable { u2c; _ } -> Some u2c
  in
  let inputs = List.filter_map var_pinput params in
  let kind_of_result, unboxable_result =
    let l =
      List.filter_map
        (fun p ->
          match p.poutput with
          | PONone -> None
          | POBoxed { ml; _ } -> Some ml
          | POUnboxable { u; _ } -> Some u)
        params
    in
    match l with
    | [] -> (UnitResult, false)
    | [ v ] -> (OneResult v, true)
    | _ -> (MultipleValues, false)
  in
  let var_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed { ml; _ } -> Some ml
    | POUnboxable { u; _ } when unboxable_result -> Some u
    | POUnboxable { ml; _ } -> Some ml
  in
  let boxed_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed { ml; _ } -> Some ml
    | POUnboxable _ when unboxable_result -> None
    | POUnboxable { ml; _ } -> Some ml
  in
  let c2a_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed { c2ml; _ } -> Some c2ml
    | POUnboxable { c2u; _ } when unboxable_result -> Some c2u
    | POUnboxable { c2ml; _ } -> Some c2ml
  in
  (* local C variable declaration *)
  let id = ID.mk ("stub_" ^ (def_of_def (def_of_code fid)).id.name) in
  fp ~kind:C ~params:inputs id (fun { fmt } ->
      (* Formals *)
      let pp_formal fmt pv = Fmt.pf fmt "value %a" pp_var pv in
      fmt "@[<hv>@[<hv 2>@[extern value %a@](%a)@[{@]@," pp_id id
        Fmt.(list ~sep:comma pp_formal)
        inputs;
      (* Local ML values *)
      let rec camlParam ~is_param ~first l =
        let add_x fmt = if first || not is_param then () else Fmt.pf fmt "x" in
        let name = if is_param then "param" else "local" in
        let pp_input fmt v = pp_var fmt v in
        let p, l =
          match l with
          | [] -> ([], [])
          | a1 :: a2 :: a3 :: a4 :: a5 :: l -> ([ a1; a2; a3; a4; a5 ], l)
          | a1 :: a2 :: a3 :: a4 :: l -> ([ a1; a2; a3; a4 ], l)
          | a1 :: a2 :: a3 :: l -> ([ a1; a2; a3 ], l)
          | a1 :: a2 :: l -> ([ a1; a2 ], l)
          | a1 :: l -> ([ a1 ], l)
        in
        if List.is_empty p then (if first then fmt "@[CAMLparam0();@]@,")
        else (
          fmt "@[CAML%t%s%i(%a);@]@," add_x name (List.length p)
            Fmt.(list ~sep:comma pp_input)
            p;
          camlParam ~is_param ~first:false l)
      in
      camlParam ~is_param:true ~first:true (List.filter_map boxed_pinput params);
      (match kind_of_result with
      | UnitResult -> ()
      | OneResult _ ->
          camlParam ~is_param:false ~first:false
            (List.filter_map boxed_poutput params)
      | MultipleValues ->
          camlParam ~is_param:false ~first:false
            (return_var :: List.filter_map boxed_poutput params));
      (* C Locals *)
      let pp_local fmt (pc, pinit_expr) =
        Fmt.pf fmt "@[%a %a%a;@]@," pp_expr pc.ty pp_var pc
          Fmt.(option (any " = " ++ pp_expr))
          pinit_expr
      in
      fmt "%a"
        Fmt.(
          list ~sep:nop (using (fun p -> p.pinit_expr) (list ~sep:nop pp_local)))
        params;
      (* convert input variables *)
      pp_scall a2c_pinput { fmt } params;
      (* initialize variables *)
      pp_scall (fun p -> p.pinit) { fmt } params;
      (* function call *)
      let pp_result fmt = function
        | None -> ()
        | Some r -> Fmt.pf fmt "%a = " pp_var r.rc
      in
      fmt "@[%a%a;@]@," pp_result result pp_call
        (fid, List.filter_map (fun p -> p.pused_in_call) params);
      (* convert output variable *)
      pp_scall c2a_poutput { fmt } params;
      (match kind_of_result with
      | UnitResult -> ()
      | OneResult _ -> ()
      | MultipleValues ->
          let l = List.filter_map var_poutput params in
          let len = List.length l in
          let li = List.mapi (fun i v -> (i, v)) l in
          (* create output tuple *)
          fmt "@[%a = caml_alloc(%i,0);@]@," pp_var return_var len;
          let pp_store fmt (i, v) =
            Fmt.pf fmt "@[Store_field(%a, %i, %a);@]" pp_var return_var i pp_var
              v
          in
          fmt "%a@]@," Fmt.(list ~sep:Fmt.cut pp_store) li);
      (* free allocated memory *)
      pp_scall (fun p -> p.pfree) { fmt } params;
      (* return *)
      (match kind_of_result with
      | UnitResult -> fmt "@[CAMLreturn(Val_unit);@]"
      | OneResult v -> fmt "@[CAMLreturnT(%a,%a);@]" pp_expr v.ty pp_var v
      | MultipleValues -> fmt "@[CAMLreturn(%a);@]" pp_var return_var);
      fmt "@]@,@[};@]@]@.")

let print_ml_fun ~params ?result ~mlname fid =
  let code_c = code_c_fun ~params ~result fid in
  let all = add_result params result in
  let inputs =
    List.filter_map
      (fun p ->
        match p.pinput with
        | PINone -> None
        | PIBoxed { pmlty; _ } -> Some (pmlty, None)
        | PIUnboxable { pmlty; unbox_attribute; _ } ->
            Some (pmlty, Some unbox_attribute))
      all
  in
  let results =
    List.filter_map
      (fun p ->
        match p.poutput with
        | PONone -> None
        | POBoxed { pmlty; _ } -> Some (pmlty, None)
        | POUnboxable { pmlty; unbox_attribute; _ } ->
            Some (pmlty, Some unbox_attribute))
      all
  in
  let pp_attribute fmt = function
    | mlty, None -> Fmt.pf fmt "%a" pp_def mlty
    | mlty, Some Untagged -> Fmt.pf fmt "(%a [@untagged])" pp_def mlty
    | mlty, Some Unboxed -> Fmt.pf fmt "(%a [@unboxed])" pp_def mlty
  in
  let one_result = List.length results = 1 in
  let pp_result fmt ((pmlty, _) as p) =
    if one_result then pp_attribute fmt p else pp_def fmt pmlty
  in
  let pp_param fmt p = Fmt.pf fmt "@[%a ->@]@ " pp_attribute p in
  expr "@[<hv 2>external %s:@ %a@[<hv>%a@]@ = \"%a\"@]" mlname
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
  let conv =
    let mk_ml2c a_ml2c =
      match a_to_b with
      | None -> code "no_a_to_b_given" ""
      | Some (a_to_b : convert) ->
          code "c2ml" "%a tmp; %a@ %a;" pp_def a.cty pp_calli
            (a_ml2c, ty_binds ~c:(expr "&tmp") a)
            pp_call
            ( a_to_b.convert,
              [ (a_to_b.src, e_var b.c); (a_to_b.dst, expr "&tmp") ] )
    in
    let mk_c2ml a_c2ml =
      match b_to_a with
      | None -> code "no_b_to_a_given" ""
      | Some (b_to_a : convert) ->
          code "c2ml" "%a tmp; %a;@ %a" pp_def a.cty pp_call
            ( b_to_a.convert,
              [ (b_to_a.src, e_var b.c); (b_to_a.dst, expr "&tmp") ] )
            pp_calli
            (a_c2ml, ty_binds ~c:(expr "&tmp") a)
    in
    match a.conv with
    | Boxed { ml2c = a_ml2c; c2ml = a_c2ml } ->
        Boxed { ml2c = mk_ml2c a_ml2c; c2ml = mk_c2ml a_c2ml }
    | Unboxable { unbox_attribute; ucty; ml2u; u2ml; u2c; c2u; u; ml2c; c2ml }
      ->
        Unboxable
          {
            unbox_attribute;
            ucty;
            ml2u;
            u2ml;
            u2c =
              (match a_to_b with
              | None -> code "no_a_to_b_given" ""
              | Some (a_to_b : convert) ->
                  code "c2ml" "%a tmp; %a@ %a;" pp_def a.cty pp_calli
                    (u2c, ty_binds ~c:(expr "&tmp") a)
                    pp_call
                    ( a_to_b.convert,
                      [ (a_to_b.src, e_var b.c); (a_to_b.dst, expr "&tmp") ] ));
            c2u =
              (match b_to_a with
              | None -> code "no_b_to_a_given" ""
              | Some (b_to_a : convert) ->
                  code "c2ml" "%a tmp; %a;@ %a" pp_def a.cty pp_call
                    ( b_to_a.convert,
                      [ (b_to_a.src, e_var b.c); (b_to_a.dst, expr "&tmp") ] )
                    pp_calli
                    (c2u, ty_binds ~c:(expr "&tmp") a));
            u;
            ml2c = mk_ml2c ml2c;
            c2ml = mk_ml2c c2ml;
          }
  in
  {
    cty = b.cty;
    mlty = a.mlty;
    mlname = a.mlname;
    conv;
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
              let _, c2ml = get_boxing ty.conv in
              Fmt.pf fmt "%a@," pp_calli
                ( c2ml,
                  ty_binds ~v:(expr "&tmp")
                    ~c:(expr "&%a->u.%s.%s" pp_var c name fname)
                    ty );
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
        cty;
        mlty;
        mlname = None;
        conv =
          Boxed { c2ml; ml2c = code ~ovars:[ v; c ] "not_yet_implemented" "" };
        init = None;
        init_expr = expr "((%a) { })" pp_def cty;
        free = None;
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
