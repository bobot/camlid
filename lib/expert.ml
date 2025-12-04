open Expr
open Type

let ty_binds ?(binds = []) ?v ?c ty =
  let bv = match v with None -> [] | Some v -> [ (ty.v, v) ] in
  let bc = match c with None -> [] | Some c -> [ (ty.cty.c, c) ] in
  bv @ bc @ binds

let e_value = expr "value"

let typedef name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> ddef ~kind:H id "typedef %t %a;@." k pp_id id)

let mlalias name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> ddef ~kind:ML id "type %a = %t@." pp_id id k)

let mlabstract ?keep_name name =
  let id = ID.mk ?keep_name name in
  ddef ~kind:ML id "type %a@." pp_id id

let existing f params =
  let id = ID.mk ~keep_name:true f in
  dfp ~params id ""

let calli_existing f params = expr "%a" pp_calli (existing f params, [])

let get_boxing = function
  | Boxed { c2ml; ml2c } -> (ml2c, c2ml)
  | Unboxable { c2ml; ml2c; _ } -> (ml2c, c2ml)

let ocaml_version =
  match String.split_on_char '.' Sys.ocaml_version with
  | major :: minor :: _ -> (int_of_string major, int_of_string minor)
  | _ -> invalid_arg "invaliad ocaml Sys.ocaml_version"

(** Native integer, the last bit is lost during translation *)
let builtin_mltypes ~unbox_attribute ~unbox_version ?u_type ~c_type ~c2ml ~ml2c
    ?(u2c = "") ?(c2u = "") ?(ml2u = ml2c) ?(u2ml = c2ml) ml_type =
  let cty = expr "%s" c_type in
  let uty = match u_type with None -> cty | Some u_type -> expr "%s" u_type in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_expr cty) in
  let u = Var.mk "c" (expr "%a" pp_expr uty) in
  let mk map x y = expr "%a = %s(%a);" pp_var x map pp_var y in
  {
    mlty = expr "%s" ml_type;
    conv =
      (if unbox_version <= ocaml_version then
         Unboxable
           {
             unbox_attribute;
             uty;
             u;
             ml2u = mk ml2u u v;
             u2ml = mk u2ml v u;
             c2u = mk c2u u c;
             u2c = mk u2c c u;
             ml2c = mk ml2c c v;
             c2ml = mk c2ml v c;
           }
       else Boxed { ml2c = mk ml2c c v; c2ml = mk c2ml v c });
    cty =
      {
        cty;
        init = None;
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        free = None;
        in_call = None;
        c;
      };
    v;
  }

(* string null terminated, convert until the first null character *)

let string_nt_alias = mlalias "string_nt" "string"

let string_nt ?(owned = true) () =
  let cty = typedef "string_nt" "char *" in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  {
    mlty = e_def string_nt_alias;
    conv =
      Boxed
        {
          c2ml = expr "%a = caml_copy_string(%a);" pp_var v pp_var c;
          ml2c =
            (let v' = Var.mk "v" (expr "value *") in
             let c' = Var.mk "c" (expr "%a *" pp_def cty) in
             call_codef "ml2c"
               [ (v', e_addr v); (c', e_addr c) ]
               (fun { fmt } ->
                 fmt "size_t len=strlen(String_val(*%a))+1;@ " pp_var v';
                 fmt "*%a=malloc(len);@ " pp_var c';
                 fmt "memcpy(*%a,String_val(*%a),len);" pp_var c' pp_var v'));
        };
    cty =
      {
        cty = e_def cty;
        init = None;
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = (if owned then Some (expr "free(%a);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
  }

let string_nt =
  let ty_owned = string_nt ~owned:true () in
  let ty_not_owned = string_nt ~owned:false () in
  fun ?(owned = true) () -> if owned then ty_owned else ty_not_owned

let string_fixed_length ?(init = true) ?(owned = true) len =
  let cty = typedef "string_fs" "char *" in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  let v' = Var.mk "v" (expr "value*") in
  let c' = Var.mk "c" (expr "%a*" pp_def cty) in
  {
    mlty = expr "%s" "string";
    conv =
      Boxed
        {
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "*%a = caml_alloc_string(%a);" pp_var v' pp_var len;
                fmt "memcpy(&Byte(*%a,0),*%a,%a);" pp_var v' pp_var c' pp_var
                  len);
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "*%a=malloc(%a);@ " pp_var c' pp_var len;
                fmt "memcpy(*%a,String_val(*%a),%a);" pp_var c' pp_var v' pp_var
                  len);
        };
    cty =
      {
        cty = e_def cty;
        init =
          (if init then Some (expr "%a =@ malloc(%a);" pp_var c pp_var len)
           else None);
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = (if owned then Some (expr "free(%a);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
  }

let string_length_struct =
  let id = ID.mk "string_s" in
  toplevel id "struct %a { char* t; size_t len; };@." pp_id id

let string_length ?(owned = true) () =
  let cty = typedef "string_length" "struct %a" pp_def string_length_struct in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    mlty = expr "%s" "string";
    conv =
      Boxed
        {
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "*%a = caml_alloc_string(%a->len);" pp_var v' pp_var c';
                fmt "memcpy(&Byte(*%a,0),%a->t,%a->len);" pp_var v' pp_var c'
                  pp_var c');
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "%a->len=caml_string_length(*%a);@ " pp_var c' pp_var v';
                fmt "%a->t=malloc(%a->len);@ " pp_var c' pp_var c';
                fmt "memcpy(%a->t,String_val(*%a),%a->len);" pp_var c' pp_var v'
                  pp_var c');
        };
    cty =
      {
        cty = e_def cty;
        init = None;
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = (if owned then Some (expr "free(%a);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
  }

let ptr_ref (ty : mlc) =
  let cty = typedef "ref" "%a *" pp_expr ty.cty.cty in
  let c = Var.mk "c" (expr "%a *" pp_def cty) in
  let add_addr e = binds [ (ty.cty.c, expr "*%a" pp_var c) ] e in
  let conv =
    match ty.conv with
    | Boxed { ml2c; c2ml } ->
        Boxed { ml2c = add_addr ml2c; c2ml = add_addr c2ml }
    | Unboxable { unbox_attribute; uty; ml2u; u2ml; u2c; c2u; u; ml2c; c2ml } ->
        Unboxable
          {
            unbox_attribute;
            uty;
            ml2u;
            u2ml;
            u2c = add_addr u2c;
            c2u = add_addr c2u;
            u;
            ml2c = add_addr ml2c;
            c2ml = add_addr c2ml;
          }
  in
  {
    mlty = ty.mlty;
    conv;
    cty =
      {
        cty = e_def cty;
        init = Option.map add_addr ty.cty.init;
        init_expr =
          expr "&(((struct { %a a; }) { %a }).a)" pp_expr ty.cty.cty pp_expr
            ty.cty.init_expr;
        free = Option.map add_addr ty.cty.free;
        in_call = None;
        c;
      };
    v = ty.v;
  }

type copy = { copy : Expr.expr; c_from : Expr.Var.t; c_to : Expr.Var.t }

module App (App : sig
  type 'a t
  type args

  val app : args -> 'a t -> 'a
  val none : args -> Var.t list
end) =
struct
  let gen ~debug_name vars exprs name (args : App.args) =
    match (vars, exprs) with
    | Some _, Some _ -> Fmt.invalid_arg "mk_%s: only vars or args" debug_name
    | None, Some (exprs : Expr.expr list App.t) ->
        expr "%s(@ %a);" name Fmt.(list ~sep:comma pp_expr) (App.app args exprs)
    | Some (vars : Expr.var list App.t), None ->
        expr "%a;" pp_call (existing name (App.app args vars), [])
    | None, None -> expr "%a;" pp_call (existing name (App.none args), [])
end

module DstSrc = App (struct
  type 'a t = dst:Var.t -> src:Var.t -> 'a
  type args = Var.t * Var.t

  let app (dst, src) f = f ~dst ~src
  let none (a, b) = [ a; b ]
end)

let mk_copy ~cty ?vars ?exprs copy =
  let c_to = Var.mk "c_to" (expr "%a*" pp_expr cty) in
  let c_from = Var.mk "c_from" (expr "%a*" pp_expr cty) in
  let copy = DstSrc.gen ~debug_name:"copy" vars exprs copy (c_to, c_from) in
  { copy; c_to; c_from }

let copy ~copy (ty : mlc) =
  let tmp = Var.mk "tmp" ty.cty.cty in
  let v' = Var.mk "v" (expr "value*") in
  let c' = Var.mk "c" (expr "%a*" pp_expr ty.cty.cty) in
  let conv =
    match ty.conv with
    | Boxed { ml2c; c2ml } ->
        Boxed
          {
            ml2c;
            c2ml =
              call_codef "c2ml"
                [ (c', e_addr ty.cty.c); (v', e_addr ty.v) ]
                ~locals:[ tmp ]
                (fun { fmt } ->
                  fmt "%a %a;@ " pp_expr tmp.ty pp_var tmp;
                  (* todo init_expr *)
                  Option.iter
                    (fun init ->
                      fmt "@[%a@]@ " pp_expr_binds
                        (init, [ (ty.cty.c, e_var tmp) ]))
                    ty.cty.init;
                  fmt "@[%a@]@ " pp_expr_binds
                    ( copy.copy,
                      [ (copy.c_from, e_var c'); (copy.c_to, e_addr tmp) ] );
                  fmt "@[%a@]@ " pp_expr_binds
                    (c2ml, [ (ty.cty.c, e_var tmp); (ty.v, e_deref v') ]);
                  Option.iter
                    (fun free ->
                      fmt "@[%a@]" pp_expr_binds
                        (free, [ (ty.cty.c, e_var tmp) ]))
                    ty.cty.free);
          }
    | Unboxable _ -> invalid_arg "not implemented"
  in
  { ty with conv }

let array ?(init = true) ?(owned = true) ~len (ty : mlc) =
  let cty = expr "%a*" pp_expr ty.cty.cty in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_expr cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  let malloc { fmt } =
    fmt "@[*%a = malloc(sizeof(%a)*%a);@]@," pp_var c' pp_expr ty.cty.cty pp_var
      len
  in
  {
    mlty = expr "(%a array)" pp_expr ty.mlty;
    conv =
      (let ml2c, c2ml = get_boxing ty.conv in
       Boxed
         {
           c2ml =
             call_codef "c2ml"
               [ (v', e_addr v); (c', e_addr c) ]
               (fun { fmt } ->
                 fmt "CAMLparam0 ();@,";
                 fmt "CAMLlocal1(cid_temp);@,";
                 fmt "*%a=caml_alloc(%a,0);@," pp_var v' pp_var len;
                 fmt
                   "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
                    ){@]@,\
                    %a@,\
                    Store_field(*%a,cid_i,cid_temp);@,\
                    }@]@,"
                   pp_var len pp_expr_binds
                   ( c2ml,
                     ty_binds ~v:(expr "cid_temp")
                       ~c:(expr "((*%a)[cid_i])" pp_var c')
                       ty )
                   pp_var v';
                 fmt "CAMLreturn0;");
           ml2c =
             call_codef "ml2c"
               [ (v', e_addr v); (c', e_addr c) ]
               (fun { fmt } ->
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
                   pp_var len pp_var v' pp_expr_binds
                   ( ml2c,
                     ty_binds ~v:(expr "cid_temp")
                       ~c:(expr "((*%a)[cid_i])" pp_var c')
                       ty );
                 fmt "CAMLreturn0;");
         });
    cty =
      {
        cty;
        init =
          (if init then
             Some
               (call_codef "init"
                  [ (v', e_addr v); (c', e_addr c) ]
                  (fun { fmt } ->
                    malloc { fmt };
                    match ty.cty.init with
                    | None -> ()
                    | Some init ->
                        fmt
                          "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
                           ){@]@,\
                           %a@,\
                           }@]@,"
                          pp_var len pp_expr_binds
                          ( init,
                            ty_binds ~c:(expr "((*%a)[cid_i])" pp_var c') ty )))
           else None);
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        free = (if owned then Some (expr "free(%a);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
  }

let array_length ?(owned = true) (ty : mlc) =
  let sstruct =
    let id = ID.mk "array_s" in
    toplevel id "struct %a { %a* t; size_t len; };@." pp_id id pp_expr
      ty.cty.cty
  in
  let cty = typedef "array" "struct %a" pp_def sstruct in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (e_def cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    mlty = expr "(%a array)" pp_expr ty.mlty;
    conv =
      (let ml2c, c2ml = get_boxing ty.conv in
       Boxed
         {
           c2ml =
             call_codef "c2ml"
               [ (v', e_addr v); (c', e_addr c) ]
               (fun { fmt } ->
                 fmt "CAMLparam0 ();@,";
                 fmt "CAMLlocal1(cid_temp);@,";
                 fmt "*%a=caml_alloc(%a->len,0);@," pp_var v' pp_var c';
                 fmt
                   "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
                    ){@]@,\
                    %a@,\
                    Store_field(*%a,cid_i,cid_temp);@,\
                    }@]@,"
                   pp_var c' pp_expr_binds
                   ( c2ml,
                     ty_binds ~v:(expr "cid_temp")
                       ~c:(expr "%a->t[cid_i]" pp_var c')
                       ty )
                   pp_var v';
                 fmt "CAMLreturn0;");
           ml2c =
             call_codef "ml2c"
               [ (v', e_addr v); (c', e_addr c) ]
               (fun { fmt } ->
                 fmt "CAMLparam0 ();@,";
                 fmt "CAMLlocal1(cid_temp);@,";
                 fmt "@[%a->len = Wosize_val(*%a);@]@," pp_var c' pp_var v';
                 fmt "@[%a->t = malloc(sizeof(%a)*%a->len);@]@," pp_var c'
                   pp_expr ty.cty.cty pp_var c';
                 fmt
                   "@[<hv 2>@[<hv 2>for(@,\
                    size_t cid_i=0;@ cid_i < %a->len;@ cid_i++@,\
                    ){@]@,\
                    cid_temp=Field(*%a,cid_i);@,\
                    %a@,\
                    }@]@,"
                   pp_var c' pp_var v' pp_expr_binds
                   ( ml2c,
                     ty_binds ~v:(expr "cid_temp")
                       ~c:(expr "%a->t[cid_i]" pp_var c')
                       ty );
                 fmt "CAMLreturn0;");
         });
    cty =
      {
        cty = e_def cty;
        init = None;
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = (if owned then Some (expr "free(%a.t);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
  }

let map_param_in_call ?(name = "arg") map param =
  match param.pused_in_call with
  | None -> invalid_arg "Not used in call"
  | Some (pcall, e) ->
      let ty, arg = map pcall.ty e in
      let pcall2 = Var.mk name ty in
      { param with pused_in_call = Some (pcall2, arg) }

let deref_in_call param =
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

let get_field ty field_name param =
  map_param_in_call ~name:"ptr"
    (fun _ e -> (ty, expr "%a.%s" pp_expr e field_name))
    param

let t_field ty param = get_field (expr "%a*" pp_expr ty.cty.cty) "t" param
let len_field param = get_field (expr "size_t") "len" param

type get = {
  get : expr;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

type set = {
  set : expr;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

type initialize = { initialize : expr; c : var }

(** Encapsulate a c type into an abstract ml type *)
let abstract ?initialize ?get ?set ~icty ~ml ~cty () =
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    v;
    mlty = e_def @@ mlabstract ~keep_name:true ml;
    conv =
      Boxed
        {
          ml2c =
            (match get with
            | None ->
                expr "%a = *((%a *) Bp_val(%a));" pp_var c pp_def icty pp_var v
            | Some f ->
                expr "%a" pp_expr_binds
                  ( f.get,
                    [
                      (f.c, e_addr c);
                      (f.i, expr "((%a *) Bp_val(%a))" pp_def icty pp_var v);
                    ] ));
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt
                  "@[*%a = caml_alloc((sizeof(%a) + sizeof(value) - 1) / \
                   sizeof(value), Abstract_tag);@]@,"
                  pp_var v' pp_def icty;
                match set with
                | None ->
                    fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_def icty pp_var v'
                      pp_var c'
                | Some f ->
                    fmt "%a" pp_expr_binds
                      ( f.set,
                        [
                          (f.c, e_var c');
                          ( f.i,
                            expr "((%a *) Bp_val(*%a))" pp_def icty pp_var v' );
                        ] ));
        };
    cty =
      {
        cty = e_def cty;
        c;
        init =
          (let pp_init fmt f =
             Fmt.pf fmt "%a" pp_expr_binds (f.initialize, [ (f.c, e_addr c) ])
           in
           expro "%a" Fmt.(option pp_init) initialize);
        init_expr = expr "((%a) { 0 })" pp_def cty;
        in_call = None;
        free = None;
      };
  }

type finalize = { finalize : expr; i : var }
type finalize_op = { finalize_op : code; v : var }
type hash = { hash : expr; i : var }
type hash_op = { hash_op : code; v : var }
type compare = { compare : expr; i1 : var; i2 : var }
type compare_op = { compare_op : code; v1 : var; v2 : var }

(** Encapsulate a c type into an custom ml type *)
let custom ?initialize ?finalize ?hash ?compare ?get ?set ~ml ~icty ~cty () =
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" cty in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  let data_custom_val icty v =
    expr "((%a *) Data_custom_val(%a))" pp_expr icty pp_var v
  in
  let data_custom_val' icty v =
    expr "((%a *) Data_custom_val(*%a))" pp_expr icty pp_var v
  in
  let finalize_op =
    match finalize with
    | None -> None
    | Some finalize ->
        let v = Var.mk "v" e_value in
        Some
          {
            finalize_op =
              code "finalize_op" "%a" pp_expr_binds
                (finalize.finalize, [ (finalize.i, data_custom_val icty v) ]);
            v;
          }
  in
  let hash_op =
    Option.map
      (fun hash ->
        let v = Var.mk "v" e_value in
        {
          hash_op =
            code ~ret:(expr "intptr_t") "hash_op" "return %a;" pp_expr_binds
              (hash.hash, [ (hash.i, data_custom_val icty v) ]);
          v;
        })
      hash
  in
  let compare_op =
    Option.map
      (fun compare ->
        let v1 = Var.mk "v1" e_value in
        let v2 = Var.mk "v2" e_value in
        {
          compare_op =
            code ~ret:(expr "int") "compare_op" "return %a;" pp_expr_binds
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
    mlty = e_def @@ mlabstract ~keep_name:true ml;
    conv =
      Boxed
        {
          ml2c =
            (match get with
            | None ->
                expr "%a = *(%a);" pp_var c pp_expr (data_custom_val icty v)
            | Some f ->
                expr "%a" pp_expr_binds
                  (f.get, [ (f.i, data_custom_val icty v); (f.c, e_addr c) ]));
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@,"
                  pp_var v' pp_def custom_op pp_expr icty;
                match set with
                | None ->
                    fmt "@[*(%a) = *%a;@]" pp_expr (data_custom_val' icty v')
                      pp_var c'
                | Some f ->
                    fmt "@[%a@]" pp_expr_binds
                      ( f.set,
                        [ (f.i, data_custom_val' icty v'); (f.c, e_var c') ] ));
        };
    cty =
      {
        cty;
        init =
          (let pp_init fmt f =
             Fmt.pf fmt "%a" pp_expr_binds (f.initialize, [ (f.c, e_addr c) ])
           in
           expro "%a" Fmt.(option pp_init) initialize);
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        free = None;
        in_call = None;
        c;
      };
    v;
  }

(** Encapsulate a c pointer type into an custom ml type *)
let custom_ptr ?initialize ?finalize ?hash ?compare ?(malloc = true) ~ml ~cty ()
    =
  let cty_ptr = expr "%a*" pp_expr cty in
  let initialize =
    let c = Var.mk "c" (expr "%a *" pp_expr cty_ptr) in
    let fmalloc { fmt } =
      fmt "*%a = malloc(sizeof(%a));" pp_var c pp_expr cty
    in
    let finit initialize { fmt } =
      fmt "%a" pp_expr_binds
        (initialize.initialize, [ (initialize.c, expr "*%a" pp_var c) ])
    in
    match initialize with
    | None when malloc -> Some { initialize = call_codef "init" [] fmalloc; c }
    | None -> None
    | Some initialize when malloc ->
        Some
          {
            initialize =
              call_codef "init" [] (fun fmt ->
                  fmalloc fmt;
                  fmt.fmt "@ ";
                  finit initialize fmt);
            c;
          }
    | Some initialize ->
        Some { initialize = call_codef "init" [] (finit initialize); c }
  in
  let finalize =
    let i = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
    let ffree { fmt } = fmt "free(*%a);" pp_var i in
    let ffinalize finalize { fmt } =
      fmt "%a" pp_expr_binds
        (finalize.finalize, [ (finalize.i, expr "*%a" pp_var i) ])
    in
    match finalize with
    | None when malloc -> Some { finalize = call_codef "init" [] ffree; i }
    | None -> None
    | Some finalize when malloc ->
        Some
          {
            finalize =
              call_codef "init" [] (fun fmt ->
                  ffinalize finalize fmt;
                  fmt.fmt "@ ";
                  ffree fmt);
            i;
          }
    | Some finalize ->
        Some { finalize = call_codef "init" [] (ffinalize finalize); i }
  in
  let hash =
    let i = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
    Option.map
      (fun hash ->
        {
          hash = expr "%a" pp_expr_binds (hash.hash, [ (hash.i, e_deref i) ]);
          i;
        })
      hash
  in
  let compare =
    let i1 = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
    let i2 = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
    Option.map
      (fun compare ->
        {
          compare =
            expr "%a" pp_expr_binds
              ( compare.compare,
                [ (compare.i1, e_deref i1); (compare.i2, e_deref i2) ] );
          i1;
          i2;
        })
      compare
  in
  custom ?initialize ?finalize ?hash ?compare ~ml ~cty:cty_ptr ~icty:cty_ptr ()

let mk_get ~icty ~cty ?vars ?exprs get =
  let c = Var.mk "c" (expr "%a *" pp_expr cty) in
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let get = DstSrc.gen ~debug_name:"get" vars exprs get (c, i) in
  { get; c; i }

let mk_set ~icty ~cty ?vars ?exprs set =
  let c = Var.mk "c" (expr "%a *" pp_expr cty) in
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let set = DstSrc.gen ~debug_name:"set" vars exprs set (i, c) in
  { set; c; i }

module Unary = App (struct
  type 'a t = Var.t -> 'a
  type args = Var.t

  let app c f = f c
  let none c = [ c ]
end)

let mk_finalize ~icty ?vars ?exprs finalize =
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let finalize = Unary.gen ~debug_name:"finalize" vars exprs finalize i in
  { finalize; i }

let mk_finalize_ptr ~icty ?vars ?exprs finalize =
  let i = Var.mk "i" icty in
  let finalize = Unary.gen ~debug_name:"finalize" vars exprs finalize i in
  { finalize; i }

let mk_hash ~icty ?vars ?exprs hash =
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let hash = Unary.gen ~debug_name:"hash" vars exprs hash i in
  { hash; i }

let mk_compare ~icty compare =
  let i1 = Var.mk "c" (expr "%a *" pp_expr icty) in
  let i2 = Var.mk "i" (expr "%a *" pp_expr icty) in
  { compare = expr "%a" pp_call (existing compare [ i1; i2 ], []); i1; i2 }

let mk_initialize ~cty ?vars ?exprs initialize =
  let c = Var.mk "c" (expr "%a *" pp_expr cty) in
  let initialize = Unary.gen ~debug_name:"initialize" vars exprs initialize c in
  { initialize; c }

let simple_param ?input_label ?(binds = []) ?(input = false) ?(output = false)
    ?(used_in_call = true) ?(name = "p") pty =
  let pc = Var.mk name pty.cty.cty in
  let pc_call = Var.mk name pty.cty.cty in
  let pv = Var.mk name e_value in
  let pv' = Var.mk (name ^ "_r") e_value in
  let bind code =
    Expr.binds ((pty.cty.c, e_var pc) :: (pty.v, e_var pv) :: binds) code
  in
  let bind' code =
    Expr.binds ((pty.cty.c, e_var pc) :: (pty.v, e_var pv') :: binds) code
  in
  let pinput, pinit, poutput =
    match pty.conv with
    | Boxed { c2ml; ml2c } ->
        let pinput, pinit =
          if input then
            ( PIBoxed
                {
                  ml = pv;
                  ml2c = bind ml2c;
                  pmlty = pty.mlty;
                  label = input_label;
                },
              None )
          else (PINone, Option.map bind pty.cty.init)
        in
        let poutput =
          if output then
            POBoxed { ml = pv'; c2ml = bind' c2ml; pmlty = pty.mlty }
          else PONone
        in
        (pinput, pinit, poutput)
    | Unboxable
        { unbox_attribute; uty; ml2u; u2ml; u2c; c2u; u; ml2c = _; c2ml } ->
        let pu = Var.mk name uty in
        let pu' = Var.mk name uty in
        let bind code =
          Expr.binds
            ((u, e_var pu)
            :: (pty.cty.c, e_var pc)
            :: (pty.v, e_var pv)
            :: binds)
            code
        in
        let bind' code =
          Expr.binds
            ((u, e_var pu')
            :: (pty.cty.c, e_var pc)
            :: (pty.v, e_var pv')
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
                  label = input_label;
                },
              None )
          else (PINone, Option.map bind pty.cty.init)
        in
        let poutput =
          if output then
            POUnboxable
              {
                unbox_attribute;
                ml = pv';
                u = pu';
                c2u = bind' c2u;
                u2ml = bind' u2ml;
                c2ml = bind' c2ml;
                pmlty = pty.mlty;
              }
          else PONone
        in
        (pinput, pinit, poutput)
  in
  let pused_in_call =
    if used_in_call then
      match pty.cty.in_call with
      | None -> Some (pc_call, e_var pc)
      | Some code -> Some (pc_call, bind code)
    else None
  in
  let pinit_expr = [ (pc, Some pty.cty.init_expr) ] in
  let pfree = Option.map bind pty.cty.free in
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
        pused_in_call = Some (result.rc, e_var result.rc);
        pfree = result.rfree;
        pinit = None;
        pinit_expr = [ (result.rc, None) ];
      }
      :: params
  | _ -> params

let return_var = Var.mk "ret" e_value

type kind_of_result =
  | UnitResult
  | MultipleValues
  | OneResultValue of var
  | OneResultUnboxed of { u : var; ml : var }

let pp_scall proj { fmt } l =
  match List.filter_map proj l with
  | [] -> ()
  | l -> fmt "%a@ " Fmt.(list ~sep:sp (box pp_expr)) l

let code_c_fun ~params ~result ~name (fid : expr) =
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
          | POBoxed { ml; _ } -> Some (ml, None)
          | POUnboxable { u; ml; _ } -> Some (ml, Some u))
        params
    in
    match l with
    | [] -> (UnitResult, false)
    | [ (v, None) ] -> (OneResultValue v, true)
    | [ (ml, Some u) ] -> (OneResultUnboxed { ml; u }, true)
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
  let unboxed_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed _ -> None
    | POUnboxable { u; _ } when unboxable_result -> Some u
    | POUnboxable _ -> None
  in
  let c2a_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed { c2ml; _ } -> Some c2ml
    | POUnboxable { c2u; _ } when unboxable_result -> Some c2u
    | POUnboxable { c2ml; _ } -> Some c2ml
  in
  (* local C variable declaration *)
  let id = ID.mk name in
  fp ~kind:C ~params:inputs id (fun { fmt } ->
      (* Formals *)
      let pp_formal fmt pv = Fmt.pf fmt "%a %a" pp_expr pv.ty pp_var pv in
      let pp_result fmt = function
        | UnitResult -> Fmt.pf fmt "value"
        | OneResultValue v | OneResultUnboxed { u = v; _ } ->
            Fmt.pf fmt "%a" pp_expr v.ty
        | MultipleValues -> Fmt.pf fmt "value"
      in
      fmt "@[<hv>@[<hv 2>@[extern %a %a@](%a)@[{@]@," pp_result kind_of_result
        pp_id id
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
        if p = [] then (if first then fmt "@[CAMLparam0();@]@,")
        else (
          fmt "@[CAML%t%s%i(%a);@]@," add_x name (List.length p)
            Fmt.(list ~sep:comma pp_input)
            p;
          camlParam ~is_param ~first:false l)
      in
      camlParam ~is_param:true ~first:true (List.filter_map boxed_pinput params);
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue _ ->
          camlParam ~is_param:false ~first:false
            (List.filter_map boxed_poutput params)
      | OneResultUnboxed _ ->
          let pp_local pc = fmt "@[%a %a;@]@," pp_expr pc.ty pp_var pc in
          List.iter pp_local (List.filter_map unboxed_poutput params)
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
      camlParam ~is_param:true ~first:false
        (List.concat_map
           (fun p ->
             List.filter_map
               (fun (c, _) -> if c.ty == e_value then Some c else None)
               p.pinit_expr)
           params);
      (* convert input variables *)
      pp_scall a2c_pinput { fmt } params;
      (* initialize variables *)
      pp_scall (fun p -> p.pinit) { fmt } params;
      (* function call *)
      fmt "@[%a@]@," pp_expr_binds
        (fid, List.filter_map (fun p -> p.pused_in_call) params);
      (* convert output variable *)
      pp_scall c2a_poutput { fmt } params;
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue _ | OneResultUnboxed _ -> ()
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
      | OneResultValue v -> fmt "@[CAMLreturn(%a);@]" pp_var v
      | OneResultUnboxed { u; _ } ->
          fmt "@[CAMLreturnT(%a,%a);@]" pp_expr u.ty pp_var u
      | MultipleValues -> fmt "@[CAMLreturn(%a);@]" pp_var return_var);
      fmt "@]@,@[}@]@]@.")

let code_c_fun_bytecode ~params ~result fid_native =
  let params = add_result params result in
  let var_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml; _ } -> Some ml
    | PIUnboxable { ml; _ } -> Some ml
  in
  let unboxed_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed _ -> None
    | PIUnboxable { u; _ } -> Some u
  in
  let ml2u_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed _ -> None
    | PIUnboxable { ml2u; _ } -> Some ml2u
  in
  let inputs = List.filter_map var_pinput params in
  let kind_of_result =
    let l =
      List.filter_map
        (fun p ->
          match p.poutput with
          | PONone -> None
          | POBoxed { ml; _ } -> Some (ml, None)
          | POUnboxable { u; ml; _ } -> Some (ml, Some u))
        params
    in
    match l with
    | [] -> UnitResult
    | [ (ml, Some u) ] -> OneResultUnboxed { ml; u }
    | [ (ml, None) ] -> OneResultValue ml
    | _ -> MultipleValues
  in
  let u2ml_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed _ -> None
    | POUnboxable { u2ml; _ } -> Some u2ml
  in
  (* local C variable declaration *)
  let id = ID.mk (name_of_def (def_of_code fid_native) ^ "_byte") in
  fp ~kind:C ~params:inputs id (fun { fmt } ->
      (* Formals *)
      if List.length inputs <= 5 then
        let pp_formal fmt pv = Fmt.pf fmt "%a %a" pp_expr pv.ty pp_var pv in
        fmt "@[<hv>@[<hv 2>@[extern value %a@](%a)@[{@]@," pp_id id
          Fmt.(list ~sep:comma pp_formal)
          inputs
      else (
        fmt "@[<hv>@[<hv 2>@[extern value %a@](value * argv, int argn)@[{@]@,"
          pp_id id;
        List.iteri
          (fun i v -> fmt "@[value %a = argv[%i];@]@ " pp_var v i)
          inputs);
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue _ -> ()
      | OneResultUnboxed { u; ml } ->
          fmt "@[%a %a;@]@ " pp_expr u.ty pp_var u;
          fmt "@[%a %a;@]@ " pp_expr ml.ty pp_var ml
      | MultipleValues -> ());
      (* C Locals *)
      let pp_local fmt pc = Fmt.pf fmt "@[%a %a;@]@," pp_expr pc.ty pp_var pc in
      fmt "%a"
        Fmt.(list ~sep:nop pp_local)
        (List.filter_map unboxed_pinput params);
      (* convert input variables *)
      pp_scall ml2u_pinput { fmt } params;
      (* function call *)
      let pp_result fmt = function
        | UnitResult -> Fmt.pf fmt "return "
        | OneResultValue _ -> Fmt.pf fmt "return "
        | OneResultUnboxed { u; _ } -> Fmt.pf fmt "%a = " pp_var u
        | MultipleValues -> Fmt.pf fmt "return "
      in
      fmt "@[%a%a;@]@," pp_result kind_of_result pp_call
        (fid_native, List.filter_map (fun p -> p.pused_in_call) params);
      (* convert output variable *)
      pp_scall u2ml_poutput { fmt } params;
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue _ -> ()
      | OneResultUnboxed { ml; _ } -> fmt "@[return %a;@]" pp_var ml
      | MultipleValues -> ());
      fmt "@]@,@[}@]@]@.")

let print_ml_fun ~params ?result ~mlname fid =
  let code_c = code_c_fun ~params ~result ~name:("stub_" ^ mlname) fid in
  let all = add_result params result in
  let inputs =
    List.filter_map
      (fun p ->
        match p.pinput with
        | PINone -> None
        | PIBoxed { label; pmlty; _ } -> Some (pmlty, None, label)
        | PIUnboxable { label; pmlty; unbox_attribute; _ } ->
            Some (pmlty, Some unbox_attribute, label))
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
  let one_result = List.length results = 1 in
  let byte_needed =
    List.length inputs > 5
    || List.exists
         (function
           | { pinput = PINone | PIBoxed _; _ } -> false
           | { pinput = PIUnboxable _; _ } -> true)
         all
    || one_result
       && List.exists
            (function
              | { poutput = PONone | POBoxed _; _ } -> false
              | { poutput = POUnboxable _; _ } -> true)
            all
  in
  let pp_label = Fmt.(option (string ++ any ":")) in
  let pp_attribute fmt = function
    | mlty, None -> Fmt.pf fmt "%a" pp_expr mlty
    | mlty, Some Untagged -> Fmt.pf fmt "(%a [@untagged])" pp_expr mlty
    | mlty, Some Unboxed -> Fmt.pf fmt "(%a [@unboxed])" pp_expr mlty
  in
  let pp_result fmt ((pmlty, _) as p) =
    if one_result then pp_attribute fmt p else pp_expr fmt pmlty
  in
  let pp_param fmt (a, b, label) =
    Fmt.pf fmt "@[%a%a ->@]@ " pp_label label pp_attribute (a, b)
  in
  let pp_byte fmt () =
    if byte_needed then
      let code_c_byte = code_c_fun_bytecode ~params ~result code_c in
      Fmt.pf fmt "\"%a\" " pp_def (def_of_code code_c_byte)
  in
  expr "@[<hv 2>external %s:@ %a@[<hv>%a@]@ = %a\"%a\"@]" mlname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "@ *@ ") pp_result)
    results pp_byte () pp_def (def_of_code code_c)

let declare_struct name fields =
  let id = ID.mk name in
  let pp_field fmt (name, ty) = Fmt.pf fmt "%a %s;" pp_expr ty name in
  toplevel ~kind:H id "@[<hv 2>struct %a {@,%a@,};@]@." pp_id id
    Fmt.(list ~sep:cut pp_field)
    fields

let if_ ?else_ cond ~then_ =
  match else_ with
  | Some else_ ->
      expr "@[<hv 2>if(%a){@ %a@ } else {@ %a@ };@]" pp_expr cond pp_expr then_
        pp_expr else_
  | None -> expr "@[<hv 2>if(%a){@ %a@ };@]" pp_expr cond pp_expr then_

let seq l = expr "%a" Fmt.(list ~sep:cut pp_expr) l

type convert = { convert : expr; src : Var.t; dst : Var.t }

let mk_converter ~(src : c) ~(dst : c) ?vars ?exprs name =
  let dst = Expr.Var.mk "dst" (expr "%a *" pp_expr dst.cty) in
  let src = Expr.Var.mk "src" (expr "%a *" pp_expr src.cty) in
  let convert = DstSrc.gen ~debug_name:"convert" vars exprs name (dst, src) in
  { dst; src; convert }

let convert ?mlc_to_c ?c_to_mlc ~(mlc : mlc) ~(c : c) () =
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr c.cty) in
  let binds = [ (v', e_addr mlc.v); (c', e_addr c.c) ] in
  let conv =
    let mk_ml2c a_ml2c =
      match mlc_to_c with
      | None -> expr "#error(\"no_ mlc_to_c given\")"
      | Some (mlc_to_c : convert) ->
          call_codef "c2ml" binds (fun { fmt } ->
              fmt "%a tmp;@ " pp_expr mlc.cty.cty;
              fmt "%a@ " pp_expr_binds
                (a_ml2c, ty_binds ~c:(expr "tmp") ~v:(e_deref v') mlc);
              fmt "%a;" pp_expr_binds
                ( mlc_to_c.convert,
                  [ (mlc_to_c.src, e_var c'); (mlc_to_c.dst, expr "&tmp") ] ))
    in
    let mk_c2ml a_c2ml =
      match c_to_mlc with
      | None -> expr "#error(\"no_ c_to_ml given\")"
      | Some (c_to_mlc : convert) ->
          call_codef "c2ml" binds (fun { fmt } ->
              fmt "%a tmp;" pp_expr mlc.cty.cty;
              fmt "%a;@ " pp_expr_binds
                ( c_to_mlc.convert,
                  [ (c_to_mlc.src, e_var c'); (c_to_mlc.dst, expr "&tmp") ] );
              fmt " %a" pp_expr_binds
                (a_c2ml, ty_binds ~c:(expr "tmp") mlc ~v:(e_deref v')))
    in
    match mlc.conv with
    | Boxed { ml2c = a_ml2c; c2ml = a_c2ml } ->
        Boxed { ml2c = mk_ml2c a_ml2c; c2ml = mk_c2ml a_c2ml }
    | Unboxable { unbox_attribute; uty; ml2u; u2ml; u2c; c2u; u; ml2c; c2ml } ->
        let u' = Var.mk "u" (expr "%a *" pp_expr uty) in
        let binds = [ (v', e_addr mlc.v); (c', e_addr c.c); (u', e_addr u) ] in
        Unboxable
          {
            unbox_attribute;
            uty;
            ml2u;
            u2ml;
            u2c =
              (match mlc_to_c with
              | None ->
                  expr "#error(\"mlc_to_c not given for %a to %a conversion\");"
                    pp_expr mlc.cty.cty pp_expr c.cty
              | Some (mlc_to_c : convert) ->
                  call_codef "c2ml" binds (fun { fmt } ->
                      fmt "%a tmp;" pp_expr mlc.cty.cty;
                      fmt "%a@ " pp_expr_binds
                        (u2c, [ (mlc.cty.c, expr "tmp"); (u, e_deref u') ]);
                      fmt "%a;" pp_expr_binds
                        ( mlc_to_c.convert,
                          [
                            (mlc_to_c.src, e_var c'); (mlc_to_c.dst, expr "&tmp");
                          ] )));
            c2u =
              (match c_to_mlc with
              | None ->
                  expr "#error(\"c_to_mlc not given for %a to %a conversion\");"
                    pp_expr mlc.cty.cty pp_expr c.cty
              | Some (c_to_mlc : convert) ->
                  call_codef "c2ml" binds (fun { fmt } ->
                      fmt "%a tmp;@ " pp_expr mlc.cty.cty;
                      fmt "%a;@ " pp_expr_binds
                        ( c_to_mlc.convert,
                          [
                            (c_to_mlc.src, e_var c'); (c_to_mlc.dst, expr "&tmp");
                          ] );
                      fmt "%a" pp_expr_binds
                        (c2u, [ (mlc.cty.c, expr "tmp"); (u, e_deref u') ])));
            u;
            ml2c = mk_ml2c ml2c;
            c2ml = mk_ml2c c2ml;
          }
  in
  { mlty = mlc.mlty; conv; cty = c; v = mlc.v }

module AlgData = struct
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * mlc) list

  type constr = {
    name : string;
    tag : ID.t;
    smart_constructor : code;
    kind : kind;
  }

  type t = {
    ty : mlc;
    constrs : constr list;
    dst_smart_constructors : Expr.var;
  }

  let algdata ml_type l : t =
    let _, l =
      List.fold_left_map
        (fun (id_cst, id_non_cst) (name, fields) ->
          let id = ID.mk (Printf.sprintf "%s_%s" ml_type name) in
          if [] = fields then
            ((id_cst + 1, id_non_cst), (name, id, KConst id_cst))
          else
            let fields =
              List.map
                (fun (n, ty) ->
                  (n, Var.mk n (expr "%a*" pp_expr ty.cty.cty), ty))
                fields
            in
            ( (id_cst, id_non_cst + 1),
              (name, id, KNonConst (id_non_cst, fields)) ))
        (0, 0) l
    in
    let cty : defined =
      let pp_field fmt (var, _, ty) =
        Fmt.pf fmt "%a %s;" pp_expr ty.cty.cty var
      in
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
      let pp_field fmt (_, _, ty) = pp_expr fmt ty.mlty in
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
    let v = Var.mk "v" (expr "value") in
    let c = Var.mk "c" (expr "%a" pp_def cty) in
    let v' = Var.mk "v" (expr "value *") in
    let c' = Var.mk "c" (expr "%a *" pp_def cty) in
    let c2ml =
      let pp_case fmt (name, id, fields) =
        Fmt.pf fmt "@[<hv 2>case %a: /* %s */@ " pp_id id name;
        (match fields with
        | KConst nc -> Fmt.pf fmt "*%a = Val_int(%i);@ " pp_var v' nc
        | KNonConst (nc, fields) ->
            let nb_fields = List.length fields in
            Fmt.pf fmt "*%a=caml_alloc(%i,%i);@ " pp_var v' nb_fields nc;
            let fields = List.mapi (fun i x -> (i, x)) fields in
            let pp_field fmt (i, (fname, _, ty)) =
              let _, c2ml = get_boxing ty.conv in
              Fmt.pf fmt "%a@," pp_expr_binds
                ( c2ml,
                  ty_binds ~v:(expr "tmp")
                    ~c:(expr "%a->u.%s.%s" pp_var c' name fname)
                    ty );
              Fmt.pf fmt "Store_field(*%a,%i,tmp);@ " pp_var v' i
            in
            Fmt.(list ~sep:sp pp_field) fmt fields);
        Fmt.pf fmt "break;@]"
      in
      call_codef "c2ml"
        [ (v', e_addr v); (c', e_addr c) ]
        (fun { fmt } ->
          fmt "CAMLparam0();@ ";
          fmt "CAMLlocal1(tmp);@ ";
          fmt "switch(%a->tag){@,%a};@ " pp_var c' Fmt.(list ~sep:sp pp_case) l;
          fmt "CAMLreturn0;")
    in
    let ty =
      {
        mlty = e_def mlty;
        conv = Boxed { c2ml; ml2c = expr "#error(\"not_yet_implemented\")" };
        cty =
          {
            cty = e_def cty;
            init = None;
            init_expr = expr "((%a) { 0 })" pp_def cty;
            free = None;
            in_call = None;
            c;
          };
        v;
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
          Expr.code ~inline:true ~kind:H ~params:[ dst ]
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
          codef ~inline:true ~kind:H fun_name ~params:(dst :: params)
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

let value =
  let cty = e_value in
  let v = Var.mk "v" e_value in
  let c = Var.mk "c" cty in
  let conv =
    Boxed
      {
        ml2c = expr "%a = %a;" pp_var c pp_var v;
        c2ml = expr "%a = %a;" pp_var v pp_var c;
      }
  in
  let init_expr = expr "Val_unit" in
  fun ml ->
    {
      mlty = expr "%s" ml;
      conv;
      cty = { cty; init = None; init_expr; free = None; in_call = None; c };
      v;
    }

let ret_option_if expr typedef =
  let ml2c, c2ml = get_boxing typedef.conv in
  let c2ml =
    let tmp = Expr.Var.mk "tmp" e_value in
    let v' = Expr.Var.mk "c" (Expr.expr "value *") in
    let c' = Expr.Var.mk "c" (Expr.expr "%a*" pp_expr typedef.cty.cty) in
    call_codef "option_if"
      [ (v', e_addr typedef.v); (c', e_addr typedef.cty.c) ]
      ~locals:[ tmp ]
      (fun { fmt } ->
        fmt "CAMLparam0();";
        fmt "CAMLlocal1(%a);@ " pp_var tmp;
        fmt "%a@ " pp_expr_binds
          (c2ml, [ (typedef.v, e_var tmp); (typedef.cty.c, e_deref c') ]);
        fmt "@[<hv 2>if(%a){@ " pp_expr expr;
        fmt "*%a=caml_alloc(1,0);@ " pp_var v';
        fmt "Store_field(*%a,0,%a); }@ " pp_var v' pp_var tmp;
        fmt "else {@ *%a = Val_unit; }" pp_var v';
        fmt "CAMLreturn0;")
  in
  {
    typedef with
    mlty = Expr.expr "(%a) option" pp_expr typedef.mlty;
    conv = Boxed { ml2c; c2ml };
  }

let simple_result rty =
  let rc = Var.mk "res" rty.cty.cty in
  let rv' = Var.mk "vres" e_value in
  let bind' code =
    Expr.binds [ (rty.cty.c, e_var rc); (rty.v, e_var rv') ] code
  in
  let routput =
    match rty.conv with
    | Boxed { c2ml; ml2c = _ } ->
        POBoxed { ml = rv'; c2ml = bind' c2ml; pmlty = rty.mlty }
    | Unboxable
        {
          unbox_attribute;
          uty;
          ml2u = _;
          u2ml;
          u2c = _;
          c2u;
          u;
          ml2c = _;
          c2ml;
        } ->
        let ru = Var.mk "ures" uty in
        let bind' code =
          Expr.binds
            [ (u, e_var ru); (rty.cty.c, e_var rc); (rty.v, e_var rv') ]
            code
        in
        POUnboxable
          {
            unbox_attribute;
            ml = rv';
            u = ru;
            c2u = bind' c2u;
            u2ml = bind' u2ml;
            c2ml = bind' c2ml;
            pmlty = rty.mlty;
          }
  in
  { routput; rc; rfree = Option.map bind' rty.cty.free }

let get_expression ~mlname ty e =
  let result = simple_result ty in
  let e = expr "%a = %a;" pp_var result.rc pp_expr e in
  print_ml_fun ~mlname ~result e ~params:[]

let bigarray_array1_aux ~managed ~kind ~cty ~mlty ~mlelt () =
  let _struct =
    let id = ID.mk "bigarray" in
    toplevel id "struct %a { %s* t; size_t len; };@." pp_id id cty
  in
  let cty = typedef "bigarray" "struct %a" pp_def _struct in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_def cty) in
  {
    mlty = expr "(%s,%s,c_layout) Bigarray.Array1.t" mlty mlelt;
    conv =
      Boxed
        {
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "%a->len=Caml_ba_array_val(*%a)->dim[0];@ " pp_var c' pp_var
                  v';
                fmt "%a->t=Caml_ba_array_val(*%a)->data;@ " pp_var c' pp_var c');
          c2ml =
            expr
              "%a = caml_ba_alloc_dims@[(%s|CAML_BA_C_LAYOUT|%s,@ 1,@ %a->t,@ \
               %a->len@]);"
              pp_var v kind managed pp_var c pp_var c;
        };
    cty =
      {
        cty = e_def cty;
        init = None;
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = None;
        in_call = None;
        c;
      };
    v;
  }

let bigarray_array1 =
  let h = Hashtbl.create 10 in
  fun ~managed ~kind ~cty ~mlty ~mlelt () ->
    let key = (managed, kind, cty, mlty, mlelt) in
    match Hashtbl.find_opt h key with
    | Some s -> s
    | None ->
        let s = bigarray_array1_aux ~managed ~kind ~cty ~mlty ~mlelt () in
        Hashtbl.add h key s;
        s
