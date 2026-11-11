open Expr
open Ty

let ty_binds ?(binds = []) ?v ?c ty =
  let bv = match v with None -> [] | Some v -> [ (ty.v, v) ] in
  let bc = match c with None -> [] | Some c -> [ (ty.cty.c, c) ] in
  bv @ bc @ binds

let map_checks ?(ml2c = Fun.id) ?(c2ml = Fun.id) (ch : checks) =
  let map f = List.map (fun (ch : check) -> { ch with cond = f ch.cond }) in
  {
    ml2c_checks = map ml2c ch.ml2c_checks;
    c2ml_checks = map c2ml ch.c2ml_checks;
  }

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
    contract =
      Ty.copied ~ml2c:Ty.noalloc
        ~c2ml:
          (if String.starts_with ~prefix:"Val_" c2ml then Ty.noalloc
           else Ty.allocates)
        ();
    checks = Ty.no_checks;
  }

let string_nt_alias = mlalias "string_nt" "string"

(* string null terminated. [~prefix:true] converts the OCaml string until its
   first null character, otherwise the conversion fails if there is one. *)
let string_nt ~prefix ~owned =
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
                 if prefix then
                   fmt "size_t len=strlen(String_val(*%a))+1;@ " pp_var v'
                 else fmt "size_t len=caml_string_length(*%a)+1;@ " pp_var v';
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
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks =
      {
        ml2c_checks =
          (if prefix then []
           else
             [
               {
                 cond = expr "!caml_string_is_c_safe(%a)" pp_var v;
                 exn = Check_failure;
                 msg = "string contains null bytes";
               };
             ]);
        c2ml_checks = [];
      };
  }

let string_nt_prefix ?(owned = true) () = string_nt ~prefix:true ~owned
let string_nt ?(owned = true) () = string_nt ~prefix:false ~owned

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
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks =
      {
        ml2c_checks =
          [
            {
              cond = expr "caml_string_length(%a) < %a" pp_var v pp_var len;
              exn = Check_invalid_argument;
              msg = "string shorter than its length";
            };
          ];
        c2ml_checks = [];
      };
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
        free = (if owned then Some (expr "free(%a.t);" pp_var c) else None);
        in_call = None;
        c;
      };
    v;
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks = Ty.no_checks;
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
    contract = ty.contract;
    checks = map_checks ~c2ml:add_addr ty.checks;
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
  { ty with conv; contract = { ty.contract with owned_by_ml = false } }

(* The checks of the elements [ty] of an array, lifted to the array: the
   OCaml array [v] and the C elements [c_elt i] for [i < c_len] *)
let array_checks ~v ~c_len ~c_elt (ty : mlc) =
  let map (ch : check) name loop =
    let id =
      codef ~ret:(expr "int") name (fun { fmt } ->
          fmt "@[<hv 2>%a@]@ return 0;" loop ch.cond)
    in
    { ch with cond = expr "%a" pp_call (id, []) }
  in
  {
    ml2c_checks =
      List.map
        (fun ch ->
          map ch "check_ml" (fun fmt cond ->
              Fmt.pf fmt
                "for(size_t cid_i=0; cid_i < Wosize_val(%a); cid_i++)@ if(%a) \
                 return 1;"
                pp_var v pp_expr
                (Expr.binds [ (ty.v, expr "Field(%a, cid_i)" pp_var v) ] cond)))
        ty.checks.ml2c_checks;
    c2ml_checks =
      List.map
        (fun ch ->
          map ch "check_c" (fun fmt cond ->
              Fmt.pf fmt
                "for(size_t cid_i=0; cid_i < %a; cid_i++)@ if(%a) return 1;"
                pp_expr c_len pp_expr
                (Expr.binds [ (ty.cty.c, c_elt) ] cond)))
        ty.checks.c2ml_checks;
  }

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
        free =
          (if owned then
             Some
               (match ty.cty.free with
               | None -> expr "free(%a);" pp_var c
               | Some free ->
                   call_codef "free"
                     [ (c', e_addr c) ]
                     (fun { fmt } ->
                       fmt
                         "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a;@ cid_i++@,\
                          ){@]@,\
                          %a@,\
                          }@]@,"
                         pp_var len pp_expr_binds
                         (free, ty_binds ~c:(expr "((*%a)[cid_i])" pp_var c') ty);
                       fmt "free(%a);" pp_var c))
           else None);
        in_call = None;
        c;
      };
    v;
    contract =
      {
        ty.contract with
        ml2c = Ty.union Ty.allocates ty.contract.ml2c;
        c2ml = Ty.union Ty.allocates ty.contract.c2ml;
      };
    checks =
      (let checks =
         array_checks ~v ~c_len:(e_var len)
           ~c_elt:(expr "%a[cid_i]" pp_var c)
           ty
       in
       {
         checks with
         ml2c_checks =
           {
             cond = expr "Wosize_val(%a) < %a" pp_var v pp_var len;
             exn = Check_invalid_argument;
             msg = "array shorter than its length";
           }
           :: checks.ml2c_checks;
       });
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
        free =
          (if owned then
             Some
               (match ty.cty.free with
               | None -> expr "free(%a.t);" pp_var c
               | Some free ->
                   call_codef "free"
                     [ (c', e_addr c) ]
                     (fun { fmt } ->
                       fmt
                         "@[<hv 2>@[for(size_t cid_i=0;@ cid_i < %a->len;@ \
                          cid_i++@,\
                          ){@]@,\
                          %a@,\
                          }@]@,"
                         pp_var c' pp_expr_binds
                         (free, ty_binds ~c:(expr "%a->t[cid_i]" pp_var c') ty);
                       fmt "free(%a->t);" pp_var c'))
           else None);
        in_call = None;
        c;
      };
    v;
    contract =
      {
        ty.contract with
        ml2c = Ty.union Ty.allocates ty.contract.ml2c;
        c2ml = Ty.union Ty.allocates ty.contract.c2ml;
      };
    checks =
      array_checks ~v ~c_len:(expr "%a.len" pp_var c)
        ~c_elt:(expr "%a.t[cid_i]" pp_var c)
        ty;
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
        pml_heap = false;
        pml2c = Ty.noalloc;
        pc2ml = Ty.noalloc;
        powned_by_ml = false;
        pcheck_in = [];
        pcheck_out = [];
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

type init = { init : expr; c : var }

(** Encapsulate a c type into an abstract ml type *)
let abstract ?init ?get ?set ~icty ~ml ~cty () =
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_expr cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  {
    v;
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks = Ty.no_checks;
    mlty = e_def @@ mlabstract ~keep_name:true ml;
    conv =
      Boxed
        {
          ml2c =
            (match get with
            | None ->
                expr "%a = *((%a *) Bp_val(%a));" pp_var c pp_expr icty pp_var v
            | Some f ->
                expr "%a" pp_expr_binds
                  ( f.get,
                    [
                      (f.c, e_addr c);
                      (f.i, expr "((%a *) Bp_val(%a))" pp_expr icty pp_var v);
                    ] ));
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt
                  "@[*%a = caml_alloc((sizeof(%a) + sizeof(value) - 1) / \
                   sizeof(value), Abstract_tag);@]@,"
                  pp_var v' pp_expr icty;
                match set with
                | None ->
                    fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_expr icty pp_var v'
                      pp_var c'
                | Some f ->
                    fmt "%a" pp_expr_binds
                      ( f.set,
                        [
                          (f.c, e_var c');
                          ( f.i,
                            expr "((%a *) Bp_val(*%a))" pp_expr icty pp_var v'
                          );
                        ] ));
        };
    cty =
      {
        cty;
        c;
        init =
          (let pp_init fmt f =
             Fmt.pf fmt "%a" pp_expr_binds (f.init, [ (f.c, e_addr c) ])
           in
           expro "%a" Fmt.(option pp_init) init);
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        in_call = None;
        free = None;
      };
  }

type clear = { clear : expr; i : var }
type hash = { hash : expr; i : var }
type compare = { compare : expr; i1 : var; i2 : var }

let custom_ops ?finalize ?compare ?hash () =
  let id = ID.mk "cops" in
  let pp_op op_name fmt = function
    | None -> Fmt.pf fmt "custom_%s_default" op_name
    | Some f -> Fmt.pf fmt "%a" pp_def (def_of_code f)
  in
  toplevel id
    "static struct custom_operations %a = {\n\
     NULL,\n\
     %a,\n\
     %a,\n\
     %a,\n\
     custom_serialize_default,\n\
     custom_deserialize_default\n\
     };@."
    pp_id id (pp_op "finalize") finalize (pp_op "compare") compare
    (pp_op "hash") hash

type mem = { mem : expr; c : var }
(** Encapsulate a c type into an custom ml type *)

let custom_gen ?(ml_extra = fun _ -> []) ?init ?clear ?hash ?compare ?mem ?get
    ?set ~ml ~icty ~cty () =
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
    Option.map
      (fun clear ->
        let v = Var.mk "v" e_value in
        code "finalize_op" "%a" pp_expr_binds
          (clear.clear, [ (clear.i, data_custom_val icty v) ]))
      clear
  in
  let hash_op =
    Option.map
      (fun hash ->
        let v = Var.mk "v" e_value in
        code ~ret:(expr "intnat") "hash_op" "return %a;" pp_expr_binds
          (hash.hash, [ (hash.i, data_custom_val icty v) ]))
      hash
  in
  let compare_op =
    Option.map
      (fun compare ->
        let v1 = Var.mk "v1" e_value in
        let v2 = Var.mk "v2" e_value in
        code ~ret:(expr "int") "compare_op" "return %a;" pp_expr_binds
          ( compare.compare,
            [
              (compare.i1, data_custom_val icty v1);
              (compare.i2, data_custom_val icty v2);
            ] ))
      compare
  in
  let custom_op =
    custom_ops ?finalize:finalize_op ?compare:compare_op ?hash:hash_op ()
  in
  {
    mlty =
      (let abs = mlabstract ~keep_name:true ml in
       match ml_extra abs with [] -> e_def abs | l -> e_def (dimplicit abs l));
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
                (match mem with
                | None ->
                    fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@,"
                      pp_var v' pp_def custom_op pp_expr icty
                | Some (mem : mem) ->
                    fmt
                      "@[*%a = caml_alloc_custom_mem(&%a,sizeof(%a),@ %a);@]@,"
                      pp_var v' pp_def custom_op pp_expr icty pp_expr_binds
                      (mem.mem, [ (mem.c, e_var c') ]));
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
             Fmt.pf fmt "%a" pp_expr_binds (f.init, [ (f.c, e_addr c) ])
           in
           expro "%a" Fmt.(option pp_init) init);
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        free = None;
        in_call = None;
        c;
      };
    v;
    contract = Ty.owned_by_ml ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks = Ty.no_checks;
  }

(** Encapsulate a c pointer type into an custom ml type *)
let custom ?init ?clear ?hash ?compare ?mem ?get ?set ~ml ~icty ~cty () =
  custom_gen ?init ?clear ?hash ?compare ?mem ?get ?set ~ml ~icty ~cty ()

let custom_ptr ?init ?clear ?hash ?compare ?mem ?dispose ?(malloc = true) ~ml
    ~cty () =
  let cty_ptr = expr "%a*" pp_expr cty in
  let init =
    let c = Var.mk "c" (expr "%a *" pp_expr cty_ptr) in
    let fmalloc { fmt } =
      fmt "*%a = malloc(sizeof(%a));" pp_var c pp_expr cty
    in
    let finit init { fmt } =
      fmt "%a" pp_expr_binds (init.init, [ (init.c, expr "*%a" pp_var c) ])
    in
    match init with
    | None when malloc -> Some { init = call_codef "init" [] fmalloc; c }
    | None -> None
    | Some init when malloc ->
        Some
          {
            init =
              call_codef "init" [] (fun fmt ->
                  fmalloc fmt;
                  fmt.fmt "@ ";
                  finit init fmt);
            c;
          }
    | Some init -> Some { init = call_codef "init" [] (finit init); c }
  in
  let clear =
    let i = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
    let ffree { fmt } = fmt "free(*%a);" pp_var i in
    let ffinalize clear { fmt } =
      fmt "%a" pp_expr_binds (clear.clear, [ (clear.i, expr "*%a" pp_var i) ])
    in
    match clear with
    | None when malloc -> Some { clear = call_codef "finalize" [] ffree; i }
    | None -> None
    | Some clear when malloc ->
        Some
          {
            clear =
              call_codef "finalize" [] (fun fmt ->
                  ffinalize clear fmt;
                  fmt.fmt "@ ";
                  ffree fmt);
            i;
          }
    | Some clear ->
        Some { clear = call_codef "finalize" [] (ffinalize clear); i }
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
  let mem =
    let c = Var.mk "c" (expr "%a *" pp_expr cty_ptr) in
    let user (mem : mem) =
      expr "%a" pp_expr_binds (mem.mem, [ (mem.c, e_deref c) ])
    in
    match mem with
    | None when malloc -> Some { mem = expr "sizeof(%a)" pp_expr cty; c }
    | None -> None
    | Some mem when malloc ->
        Some { mem = expr "sizeof(%a) + %a" pp_expr cty pp_expr (user mem); c }
    | Some mem -> Some { mem = user mem; c }
  in
  match dispose with
  | None ->
      custom ?init ?clear ?hash ?compare ?mem ~ml ~cty:cty_ptr ~icty:cty_ptr ()
  | Some dispose ->
      (* the pointer is set to NULL by dispose *)
      let clear =
        Option.map
          (fun (clear : clear) ->
            {
              clear with
              clear =
                expr "@[if(*%a != NULL){@ %a@ }@]" pp_var clear.i pp_expr
                  clear.clear;
            })
          clear
      in
      let get =
        let c = Var.mk "c" (expr "%a *" pp_expr cty_ptr) in
        let i = Var.mk "i" (expr "%a *" pp_expr cty_ptr) in
        { get = expr "*%a = *%a;" pp_var c pp_var i; c; i }
      in
      let ml_extra abs =
        let v = Var.mk "v" e_value in
        let p = Var.mk "p" (expr "%a *" pp_expr cty_ptr) in
        let id = ID.mk dispose in
        let stub =
          fp ~kind:C ~params:[ v ] id (fun { fmt } ->
              fmt "@[<v 2>extern value %a(value %a){@," pp_id id pp_var v;
              fmt "%a %a = (%a) Data_custom_val(%a);@," pp_expr p.ty pp_var p
                pp_expr p.ty pp_var v;
              (match clear with
              | None -> ()
              | Some clear ->
                  fmt "@[%a@]@," pp_expr_binds
                    (clear.clear, [ (clear.i, e_var p) ]));
              fmt "*%a = NULL;@," pp_var p;
              fmt "return Val_unit;@]@,}@.")
        in
        let ml_id = ID.mk ~keep_name:true dispose in
        [
          toplevel_callable ~kind:ML ml_id
            "@[external %a : %a -> unit = \"%a\" [@@@@noalloc]@]@." pp_id ml_id
            pp_def abs pp_def (def_of_code stub);
        ]
      in
      let t =
        custom_gen ~ml_extra ?init ?clear ?hash ?compare ?mem ~get ~ml
          ~cty:cty_ptr ~icty:cty_ptr ()
      in
      {
        t with
        checks =
          {
            t.checks with
            ml2c_checks =
              [
                {
                  cond =
                    expr "*((%a *) Data_custom_val(%a)) == NULL" pp_expr cty_ptr
                      pp_var t.v;
                  exn = Check_invalid_argument;
                  msg = ml ^ ": used after " ^ dispose;
                };
              ];
          };
      }

let mk_mem ~cty name =
  let c = Var.mk "c" (expr "%a *" pp_expr cty) in
  { mem = expr "%a" pp_call (existing name [ c ], []); c }

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

let mk_clear ~icty ?vars ?exprs clear =
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let clear = Unary.gen ~debug_name:"clear" vars exprs clear i in
  { clear; i }

let mk_hash ~icty ?vars ?exprs hash =
  let i = Var.mk "i" (expr "%a *" pp_expr icty) in
  let hash = Unary.gen ~debug_name:"hash" vars exprs hash i in
  { hash; i }

let mk_compare ~icty compare =
  let i1 = Var.mk "c" (expr "%a *" pp_expr icty) in
  let i2 = Var.mk "i" (expr "%a *" pp_expr icty) in
  { compare = expr "%a" pp_call (existing compare [ i1; i2 ], []); i1; i2 }

let mk_init ~cty ?vars ?exprs init =
  let c = Var.mk "c" (expr "%a *" pp_expr cty) in
  let init = Unary.gen ~debug_name:"init" vars exprs init c in
  { init; c }

let param ?input_label ?(binds = []) ?(input = false) ?(output = false)
    ?(used_in_call = true) ?(name = "p") pty =
  if input && output && pty.contract.owned_by_ml then
    invalid_arg
      "Camlid: a parameter can't be both an input and an output when its C \
       value is owned by the OCaml value (it would be owned by two OCaml \
       values). Use an input: in-place modifications are visible.";
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
  let pml_heap = used_in_call && pty.contract.ml_heap in
  let pml2c =
    if not input then Ty.noalloc
    else
      match pty.conv with
      | Unboxable _ -> Ty.noalloc
      | Boxed _ -> pty.contract.ml2c
  in
  let pc2ml = if not output then Ty.noalloc else pty.contract.c2ml in
  let bind_check bind (ch : check) = { ch with cond = bind ch.cond } in
  let pcheck_in =
    if input then List.map (bind_check bind) pty.checks.ml2c_checks else []
  in
  let pcheck_out =
    if output then List.map (bind_check bind') pty.checks.c2ml_checks else []
  in
  ( {
      pinput;
      pinit_expr;
      pinit;
      pused_in_call;
      pfree;
      poutput;
      pml_heap;
      pml2c;
      pc2ml;
      powned_by_ml = pty.contract.owned_by_ml;
      pcheck_in;
      pcheck_out;
    },
    pc,
    pv )

let simple_param ?input_label ?binds ?input ?output ?used_in_call ?name pty =
  let param, c_var, _ =
    param ?input_label ?binds ?input ?output ?used_in_call ?name pty
  in
  (param, c_var)

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

let result_param result =
  {
    pinput = PINone;
    poutput = result.routput;
    pused_in_call = Some (result.rc, e_var result.rc);
    pfree = result.rfree;
    pinit = None;
    pinit_expr = [ (result.rc, None) ];
    pml_heap = false;
    pml2c = Ty.noalloc;
    pc2ml = result.rc2ml;
    powned_by_ml = result.rowned_by_ml;
    pcheck_in = [];
    pcheck_out = result.rcheck;
  }

let add_result params result =
  match result with Some r -> result_param r :: params | None -> params

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

type exn_ = { exn_def : defined; registered : string }

let declare_exception ?registered name =
  let registered = Option.value ~default:name registered in
  let id = ID.mk ~keep_name:true name in
  let exn_def =
    ddef ~kind:ML id
      "@[exception %a of string@]@.@[let () = Callback.register_exception %S \
       (%a \"\")@]@."
      pp_id id registered pp_id id
  in
  { exn_def; registered }

type error = { cond : expr; msg : expr option; exn : exn_ option }

let err_var = Var.mk "err" e_value

(** Moves the outputs owned by OCaml into OCaml values, which will be finalized,
    then frees *)
let pp_cleanup ~moves ~frees { fmt } =
  let move p =
    match p.poutput with
    | PONone -> None
    | POBoxed { c2ml; _ } | POUnboxable { c2ml; _ } -> Some c2ml
  in
  pp_scall move { fmt } moves;
  pp_scall (fun p -> p.pfree) { fmt } frees

let pp_check_exn fmt = function
  | Check_failure -> Fmt.string fmt "caml_failwith"
  | Check_invalid_argument -> Fmt.string fmt "caml_invalid_argument"

let pp_check_in { fmt } p =
  List.iter
    (fun (ch : check) ->
      fmt "@[if(%a) %a(%S);@]@ " pp_expr ch.cond pp_check_exn ch.exn ch.msg)
    p.pcheck_in

let pp_check_out ~moves ~frees { fmt } p =
  List.iter
    (fun (ch : check) ->
      fmt "@[<v 2>if(%a){@," pp_expr ch.cond;
      fmt "@[%a = caml_copy_string(%S);@]@," pp_var err_var ch.msg;
      pp_cleanup ~moves ~frees { fmt };
      fmt "@[%a_value(%a);@]" pp_check_exn ch.exn pp_var err_var;
      fmt "@]@,}@,")
    p.pcheck_out

let pp_error ~name ~moves ~frees { fmt } error =
  fmt "@[<v 2>if(%a){@," pp_expr error.cond;
  (match error.msg with
  | None ->
      fmt "@[%a = caml_copy_string(%S);@]@," pp_var err_var (name ^ " failed")
  | Some msg ->
      fmt "@[%a = caml_copy_string(%a);@]@," pp_var err_var pp_expr msg);
  pp_cleanup ~moves ~frees { fmt };
  (match error.exn with
  | None -> fmt "@[caml_failwith_value(%a);@]" pp_var err_var
  | Some exn ->
      fmt "@[/* %a */@]@," pp_def exn.exn_def;
      fmt
        "@[const value *exn = caml_named_value(%S);@]@,\
         @[if(exn == NULL) caml_invalid_argument(%S);@]@,\
         @[caml_raise_with_arg(*exn, %a);@]"
        exn.registered
        ("exception " ^ exn.registered ^ " not registered")
        pp_var err_var);
  fmt "@]@,}@,"

let plan_param (p : param) : Plan.param =
  {
    input =
      (match p.pinput with
      | PINone -> No_input
      | PIBoxed _ -> Boxed_input
      | PIUnboxable _ -> Unboxed_input);
    output =
      (match p.poutput with
      | PONone -> No_output
      | POBoxed _ -> Boxed_output
      | POUnboxable _ -> Unboxable_output);
    owned_by_ml = p.powned_by_ml;
    init = Option.is_some p.pinit;
    free = Option.is_some p.pfree;
    value_locals = List.exists (fun (c, _) -> c.ty == e_value) p.pinit_expr;
    ml_heap = p.pml_heap;
    ml2c = p.pml2c;
    c2ml = p.pc2ml;
    in_check = p.pcheck_in <> [];
    out_check = p.pcheck_out <> [];
  }

(** The parameters (with the result first) and the plan of a stub *)
let plan ?noalloc ?(release_lock = false) ?error ~params ~call_params ~result
    ~name () =
  let result = Option.map result_param result in
  let params = Option.to_list result @ params in
  let call_params = Option.to_list result @ call_params in
  let index p =
    let rec aux i = function
      | [] ->
          Fmt.invalid_arg
            "Camlid: in %s, a parameter of the call is not a parameter of the \
             function"
            name
      | q :: _ when q == p -> i
      | _ :: l -> aux (i + 1) l
    in
    aux 0 params
  in
  let options : Plan.options =
    { want_noalloc = noalloc; release_lock; error = Option.is_some error }
  in
  let pparams = List.map plan_param params in
  let call = List.map index call_params in
  match Plan.schedule ~name options pparams ~call with
  | Error msg -> invalid_arg msg
  | Ok plan -> (
      match Blocks.check options pparams ~call plan with
      | Ok () -> (params, plan)
      | Error msg ->
          Fmt.invalid_arg "Camlid: internal error, unsafe stub for %s: %s" name
            msg)

(** Prints the plan of a stub. [params] contains the result. *)
let code_c_fun_of_plan ?error ~(plan : Plan.plan) ~params ~name (fid : expr) =
  let noalloc = plan.noalloc in
  let ps = Array.of_list params in
  let get l = List.map (fun i -> ps.(i)) l in
  let var_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml; _ } -> Some ml
    | PIUnboxable { u; _ } -> Some u
  in
  let a2c_pinput p =
    match p.pinput with
    | PINone -> None
    | PIBoxed { ml2c; _ } -> Some ml2c
    | PIUnboxable { u2c; _ } -> Some u2c
  in
  let inputs = List.filter_map var_pinput params in
  let ml_poutput p =
    match p.poutput with
    | PONone -> assert false
    | POBoxed { ml; _ } | POUnboxable { ml; _ } -> ml
  in
  let kind_of_result =
    match plan.result with
    | Unit_result -> UnitResult
    | One_value i -> OneResultValue (ml_poutput ps.(i))
    | One_unboxed i -> (
        match ps.(i).poutput with
        | POUnboxable { u; ml; _ } -> OneResultUnboxed { ml; u }
        | _ -> assert false)
    | Tuple _ -> MultipleValues
  in
  let unboxable_result =
    match plan.result with
    | One_value _ | One_unboxed _ -> true
    | Unit_result | Tuple _ -> false
  in
  let c2a_poutput p =
    match p.poutput with
    | PONone -> None
    | POBoxed { c2ml; _ } -> Some c2ml
    | POUnboxable { c2u; _ } when unboxable_result -> Some c2u
    | POUnboxable { c2ml; _ } -> Some c2ml
  in
  let boxed_pinput p =
    match p.pinput with PIBoxed { ml; _ } -> ml | _ -> assert false
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
        Fmt.(list_or_empty ~empty:(any "void") ~sep:comma pp_formal)
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
        if noalloc then ()
        else if p = [] then (if first then fmt "@[CAMLparam0();@]@,")
        else (
          fmt "@[CAML%t%s%i(%a);@]@," add_x name (List.length p)
            Fmt.(list ~sep:comma pp_input)
            p;
          camlParam ~is_param ~first:false l)
      in
      let roots = plan.roots in
      camlParam ~is_param:true ~first:true
        (List.map boxed_pinput (get roots.inputs));
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue v when noalloc -> fmt "@[value %a;@]@," pp_var v
      | OneResultUnboxed { u; _ } -> fmt "@[%a %a;@]@," pp_expr u.ty pp_var u
      | OneResultValue _ | MultipleValues ->
          camlParam ~is_param:false ~first:false
            ((if roots.ret then [ return_var ] else [])
            @ List.map ml_poutput (get roots.outputs)));
      if roots.err then camlParam ~is_param:false ~first:false [ err_var ];
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
           (get roots.c_values));
      let step : Plan.step -> unit = function
        | Poll -> fmt "@[caml_process_pending_actions();@]@,"
        | Check_in l -> List.iter (pp_check_in { fmt }) (get l)
        | Ml2c l -> pp_scall a2c_pinput { fmt } (get l)
        | Init l -> pp_scall (fun p -> p.pinit) { fmt } (get l)
        | Call (l, release_lock) ->
            (* no OCaml code here: the stub can own something, and an exception
               would leak it. Poll has run the signal handlers. *)
            if release_lock then
              fmt "@[caml_enter_blocking_section_no_pending();@]@,";
            fmt "@[%a@]@," pp_expr_binds
              (fid, List.filter_map (fun p -> p.pused_in_call) (get l));
            if release_lock then fmt "@[caml_leave_blocking_section();@]@,"
        | Check_error { moves; frees } ->
            pp_error ~name ~moves:(get moves) ~frees:(get frees) { fmt }
              (Option.get error)
        | Check_out (l, { moves; frees }) ->
            List.iter
              (pp_check_out ~moves:(get moves) ~frees:(get frees) { fmt })
              (get l)
        | C2ml l -> pp_scall c2a_poutput { fmt } (get l)
        | Alloc_tuple l ->
            let li = List.mapi (fun i v -> (i, ml_poutput v)) (get l) in
            fmt "@[%a = caml_alloc(%i,0);@]@," pp_var return_var
              (List.length li);
            let pp_store fmt (i, v) =
              Fmt.pf fmt "@[Store_field(%a, %i, %a);@]" pp_var return_var i
                pp_var v
            in
            fmt "%a@]@," Fmt.(list ~sep:Fmt.cut pp_store) li
        | Free l -> pp_scall (fun p -> p.pfree) { fmt } (get l)
        | Return -> (
            match kind_of_result with
            | UnitResult when noalloc -> fmt "@[return Val_unit;@]"
            | OneResultValue v when noalloc -> fmt "@[return %a;@]" pp_var v
            | OneResultUnboxed { u; _ } when noalloc ->
                fmt "@[return %a;@]" pp_var u
            | UnitResult -> fmt "@[CAMLreturn(Val_unit);@]"
            | OneResultValue v -> fmt "@[CAMLreturn(%a);@]" pp_var v
            | OneResultUnboxed { u; _ } ->
                fmt "@[CAMLreturnT(%a,%a);@]" pp_expr u.ty pp_var u
            | MultipleValues -> fmt "@[CAMLreturn(%a);@]" pp_var return_var)
      in
      List.iter step plan.steps;
      fmt "@]@,@[}@]@]@.")

let code_c_fun ?noalloc ?release_lock ?error ~params ~call_params ~result ~name
    fid =
  let params, plan =
    plan ?noalloc ?release_lock ?error ~params ~call_params ~result ~name ()
  in
  code_c_fun_of_plan ?error ~plan ~params ~name fid

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
          Fmt.(list_or_empty ~empty:(any "void") ~sep:comma pp_formal)
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
      (match kind_of_result with
      | UnitResult -> ()
      | OneResultValue _ -> ()
      | OneResultUnboxed { ml; _ } ->
          pp_scall u2ml_poutput { fmt } params;
          fmt "@[return %a;@]" pp_var ml
      | MultipleValues -> ());
      fmt "@]@,@[}@]@]@.")

let print_ml_fun ?noalloc ?release_lock ?error ~params ?(call_params = params)
    ?result ~mlname fid =
  let all_params, plan =
    plan ?noalloc ?release_lock ?error ~params ~call_params ~result ~name:mlname
      ()
  in
  let noalloc = plan.noalloc in
  let code_c =
    code_c_fun_of_plan ?error ~plan ~params:all_params ~name:("stub_" ^ mlname)
      fid
  in
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
  expr "@[<hv 2>external %s:@ %a@[<hv>%a@]@ = %a\"%a\"%s@]" mlname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "@ *@ ") pp_result)
    results pp_byte () pp_def (def_of_code code_c)
    (if noalloc then " [@@noalloc]" else "")

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

(* The conversion goes through a temporary [tmp] of the C type of [mlc]. Like
   any C value, [tmp] and the destination are initialized before being
   assigned, and [tmp] is freed after use. *)
let convert ?mlc_to_c ?c_to_mlc ~(mlc : mlc) ~(c : c) () =
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr c.cty) in
  let tmp = Var.mk "tmp" mlc.cty.cty in
  let on_tmp e = binds [ (mlc.cty.c, e_var tmp) ] e in
  let declare_tmp { fmt } =
    fmt "%a %a = %a;@ " pp_expr mlc.cty.cty pp_var tmp pp_expr mlc.cty.init_expr
  in
  let free_tmp { fmt } =
    Option.iter (fun free -> fmt "@ %a" pp_expr (on_tmp free)) mlc.cty.free
  in
  (* [load] fills [tmp] from the OCaml value *)
  let to_c binds load =
    match mlc_to_c with
    | None -> expr "#error(\"mlc_to_c not given\")"
    | Some (mlc_to_c : convert) ->
        call_codef "ml2c" binds ~locals:[ tmp ] (fun { fmt } ->
            declare_tmp { fmt };
            fmt "%a@ " pp_expr (on_tmp load);
            Option.iter
              (fun init ->
                fmt "%a@ " pp_expr_binds (init, [ (c.c, e_deref c') ]))
              c.init;
            fmt "%a;" pp_expr_binds
              ( mlc_to_c.convert,
                [ (mlc_to_c.dst, e_var c'); (mlc_to_c.src, e_addr tmp) ] );
            free_tmp { fmt })
  in
  (* [store] converts [tmp] to the OCaml value *)
  let to_ml binds store =
    match c_to_mlc with
    | None -> expr "#error(\"c_to_mlc not given\")"
    | Some (c_to_mlc : convert) ->
        call_codef "c2ml" binds ~locals:[ tmp ] (fun { fmt } ->
            declare_tmp { fmt };
            Option.iter
              (fun init -> fmt "%a@ " pp_expr (on_tmp init))
              mlc.cty.init;
            fmt "%a;@ " pp_expr_binds
              ( c_to_mlc.convert,
                [ (c_to_mlc.src, e_var c'); (c_to_mlc.dst, e_addr tmp) ] );
            fmt "%a" pp_expr (on_tmp store);
            free_tmp { fmt })
  in
  let binds = [ (v', e_addr mlc.v); (c', e_addr c.c) ] in
  let on_v e = Expr.binds [ (mlc.v, e_deref v') ] e in
  let conv =
    match mlc.conv with
    | Boxed { ml2c; c2ml } ->
        Boxed { ml2c = to_c binds (on_v ml2c); c2ml = to_ml binds (on_v c2ml) }
    | Unboxable { unbox_attribute; uty; ml2u; u2ml; u2c; c2u; u; ml2c; c2ml } ->
        let u' = Var.mk "u" (expr "%a *" pp_expr uty) in
        let ubinds = [ (u', e_addr u); (c', e_addr c.c) ] in
        let on_u e = Expr.binds [ (u, e_deref u') ] e in
        Unboxable
          {
            unbox_attribute;
            uty;
            ml2u;
            u2ml;
            u2c = to_c ubinds (on_u u2c);
            c2u = to_ml ubinds (on_u c2u);
            u;
            ml2c = to_c binds (on_v ml2c);
            c2ml = to_ml binds (on_v c2ml);
          }
  in
  {
    mlty = mlc.mlty;
    conv;
    cty = c;
    v = mlc.v;
    contract = { mlc.contract with owned_by_ml = false; ml_heap = false };
    checks =
      (if mlc.checks.c2ml_checks <> [] then
         invalid_arg
           "Camlid.convert: the conversion of an output with a check is not \
            supported";
       { mlc.checks with c2ml_checks = [] });
  }

module AlgData = struct
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * mlc) list

  let all_fields l =
    List.concat_map
      (function
        | _, _, KConst _ -> []
        | _, _, KNonConst (_, fields) -> List.map (fun (_, _, ty) -> ty) fields)
      l

  (* Frees the fields of the constructor of [c] *)
  let algdata_free ~cty ~c l =
    let frees =
      List.filter_map
        (function
          | _, _, KConst _ -> None
          | name, id, KNonConst (_, fields) -> (
              match
                List.filter_map
                  (fun (fname, _, ty) ->
                    Option.map (fun free -> (fname, ty, free)) ty.cty.free)
                  fields
              with
              | [] -> None
              | l -> Some (name, id, l)))
        l
    in
    if frees = [] then None
    else
      let c' = Var.mk "c" (expr "%a *" pp_def cty) in
      Some
        (call_codef "free"
           [ (c', e_addr c) ]
           (fun { fmt } ->
             fmt "@[<v 2>switch(%a->tag){@," pp_var c';
             List.iter
               (fun (name, id, fields) ->
                 fmt "@[<v 2>case %a:@," pp_id id;
                 List.iter
                   (fun (fname, ty, free) ->
                     fmt "%a@," pp_expr_binds
                       ( free,
                         [ (ty.cty.c, expr "%a->u.%s.%s" pp_var c' name fname) ]
                       ))
                   fields;
                 fmt "break;@]@,")
               frees;
             fmt "default: break;@]@,}"))

  (* The checks of the fields, lifted to the whole value *)
  let algdata_checks ~ml_type ~v ~c l =
    let field_checks proj =
      List.concat_map
        (function
          | _, _, KConst _ -> []
          | name, id, KNonConst (nc, fields) ->
              List.concat_map
                (fun (i, (fname, _, ty)) ->
                  List.map
                    (fun ch -> (name, id, nc, i, fname, ty, ch))
                    (proj ty))
                (List.mapi (fun i f -> (i, f)) fields))
        l
    in
    let summary checks =
      match checks with
      | [] -> None
      | (_, _, _, _, _, _, (ch : check)) :: _ ->
          if
            List.for_all
              (fun (_, _, _, _, _, _, (ch' : check)) ->
                ch'.exn = ch.exn && ch'.msg = ch.msg)
              checks
          then Some (ch.exn, ch.msg)
          else Some (Check_failure, "invalid value of " ^ ml_type)
    in
    let ml2c_checks =
      let checks = field_checks (fun ty -> ty.checks.ml2c_checks) in
      Option.map
        (fun (exn, msg) ->
          let v' = Var.mk "v" (expr "value") in
          let id =
            codef ~ret:(expr "int") "check_ml" (fun { fmt } ->
                fmt "@[if(Is_long(%a)) return 0;@]@ " pp_var v';
                List.iter
                  (fun (_, _, nc, i, _, ty, (ch : check)) ->
                    fmt "@[if(Tag_val(%a) == %i && (%a)) return 1;@]@ " pp_var
                      v' nc pp_expr
                      (Expr.binds
                         [ (ty.v, expr "Field(%a, %i)" pp_var v' i) ]
                         ch.cond))
                  checks;
                fmt "return 0;")
          in
          ({ cond = expr "%a" pp_call (id, [ (v', e_var v) ]); exn; msg }
            : check))
        (summary checks)
    in
    let c2ml_checks =
      let checks = field_checks (fun ty -> ty.checks.c2ml_checks) in
      Option.map
        (fun (exn, msg) ->
          let c' = Var.mk "c" (expr "%a *" pp_expr c.ty) in
          let id =
            codef ~ret:(expr "int") "check_c" (fun { fmt } ->
                List.iter
                  (fun (name, id, _, _, fname, ty, (ch : check)) ->
                    fmt "@[if(%a->tag == %a && (%a)) return 1;@]@ " pp_var c'
                      pp_id id pp_expr
                      (Expr.binds
                         [ (ty.cty.c, expr "%a->u.%s.%s" pp_var c' name fname) ]
                         ch.cond))
                  checks;
                fmt "return 0;")
          in
          ({ cond = expr "%a" pp_call (id, [ (c', e_addr c) ]); exn; msg }
            : check))
        (summary checks)
    in
    {
      ml2c_checks = Option.to_list ml2c_checks;
      c2ml_checks = Option.to_list c2ml_checks;
    }

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
    (* the fields with a destructor are freed with the value; the tag
       [none] of the initial value has no field *)
    let none =
      if List.exists (fun ty -> Option.is_some ty.cty.free) (all_fields l) then
        Some (ID.mk (ml_type ^ "_none"))
      else None
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
      (* pattern matching macros, similar to datatype99 *)
      let pp_macros fmt () =
        Fmt.pf fmt
          "#define %a_match(x) for(%a *cid_m = (x); cid_m != NULL; cid_m = \
           NULL) switch((int)cid_m->tag) case -1:@\n"
          pp_id id pp_id id;
        Fmt.pf fmt "#define %a_otherwise break; default:@\n" pp_id id;
        List.iter
          (fun (name, tag, fields) ->
            match fields with
            | KConst _ ->
                Fmt.pf fmt "#define %a_of_%s() break; case %a:@\n" pp_id id name
                  pp_id tag
            | KNonConst (_, fields) ->
                Fmt.pf fmt
                  "#define %a_of_%s(%a) break; case %a: \\@\n\
                  \  for(int cid_once = 1; cid_once; cid_once = 0)"
                  pp_id id name
                  Fmt.(
                    list ~sep:(const string ", ")
                      (using (fun (n, _, _) -> n) string))
                  fields pp_id tag;
                List.iter
                  (fun (n, _, ty) ->
                    Fmt.pf fmt
                      " \\@\n\
                      \  for(%a *%s = &cid_m->u.%s.%s; cid_once; cid_once = 0)"
                      pp_expr ty.cty.cty n name n)
                  fields;
                Fmt.pf fmt "@\n")
          l
      in
      toplevel ~kind:H id
        "@[<hv 2>@[typedef struct {@]@ @[<hv 2>enum {@ %a} tag;@]@ @[<hv \
         2>union {@ %a} u;@] @[}@] %a;@]@\n\
         %a"
        Fmt.(pair ~sep:nop (list ~sep:comma pp_enum) (option (comma ++ pp_id)))
        (l, none)
        Fmt.(list ~sep:sp pp_constr)
        l pp_id id pp_macros ()
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
          fmt "switch(%a->tag){@,%a%t};@ " pp_var c'
            Fmt.(list ~sep:sp pp_case)
            l
            (fun fmt ->
              if Option.is_some none then Fmt.pf fmt "@ default: break;");
          fmt "CAMLreturn0;")
    in
    let ml2c =
      let pp_case_const fmt (name, id, nc) =
        Fmt.pf fmt "@[<hv 2>case %i: /* %s */@ " nc name;
        Fmt.pf fmt "%a->tag = %a;@ " pp_var c' pp_id id;
        Fmt.pf fmt "break;@]"
      in
      let pp_case_non_const fmt (name, id, nc, fields) =
        Fmt.pf fmt "@[<hv 2>case %i: /* %s */@ " nc name;
        (Fmt.pf fmt "%a->tag=%a;@ " pp_var c' pp_id id;
         let fields = List.mapi (fun i x -> (i, x)) fields in
         let pp_field fmt (i, (fname, _, ty)) =
           let ml2c, _ = get_boxing ty.conv in
           Fmt.pf fmt "tmp=Field(*%a,%i);@ " pp_var v' i;
           Fmt.pf fmt "%a" pp_expr_binds
             ( ml2c,
               ty_binds ~v:(expr "tmp")
                 ~c:(expr "%a->u.%s.%s" pp_var c' name fname)
                 ty )
         in
         Fmt.(list ~sep:sp pp_field) fmt fields);
        Fmt.pf fmt "@ break;@]"
      in
      let consts =
        List.filter_map
          (function name, id, KConst nc -> Some (name, id, nc) | _ -> None)
          l
      in
      let non_consts =
        List.filter_map
          (function
            | name, id, KNonConst (nc, fields) -> Some (name, id, nc, fields)
            | _ -> None)
          l
      in
      let print_consts { fmt } =
        fmt "switch(Long_val(*%a)){@,%a@,};@ " pp_var v'
          Fmt.(list ~sep:sp pp_case_const)
          consts;
        fmt "return;"
      in
      let print_non_consts { fmt } =
        fmt "CAMLparam0();@ ";
        fmt "CAMLlocal1(tmp);@ ";
        fmt "switch(Tag_val(*%a)){@,%a@,};@ " pp_var v'
          Fmt.(list ~sep:sp pp_case_non_const)
          non_consts;
        fmt "CAMLreturn0;"
      in
      call_codef "ml2c"
        [ (v', e_addr v); (c', e_addr c) ]
        (fun { fmt } ->
          match (consts, non_consts) with
          | [], [] ->
              failwith
                "Really an empty types? Please report if really needed, thanks."
          | _, [] -> print_consts { fmt }
          | [], _ -> print_non_consts { fmt }
          | _, _ ->
              fmt "if(Is_long(*%a)){" pp_var v';
              print_consts { fmt };
              fmt "}else{";
              print_non_consts { fmt };
              fmt "}")
    in
    let ty =
      {
        mlty = e_def mlty;
        conv = Boxed { c2ml; ml2c };
        cty =
          {
            cty = e_def cty;
            init = None;
            init_expr =
              (match none with
              | None -> expr "((%a) { 0 })" pp_def cty
              | Some none -> expr "((%a) { .tag = %a })" pp_def cty pp_id none);
            free = algdata_free ~cty ~c l;
            in_call = None;
            c;
          };
        v;
        contract =
          {
            owned_by_ml =
              List.exists
                (fun (_, _, k) ->
                  match k with
                  | KConst _ -> false
                  | KNonConst (_, l) ->
                      List.exists (fun (_, _, ty) -> ty.contract.owned_by_ml) l)
                l;
            ml_heap =
              List.exists
                (fun (_, _, k) ->
                  match k with
                  | KConst _ -> false
                  | KNonConst (_, l) ->
                      List.exists (fun (_, _, ty) -> ty.contract.ml_heap) l)
                l;
            ml2c =
              List.fold_left
                (fun acc ty -> Ty.union acc ty.contract.ml2c)
                Ty.allocates (all_fields l);
            c2ml =
              List.fold_left
                (fun acc ty -> Ty.union acc ty.contract.c2ml)
                Ty.allocates (all_fields l);
          };
        checks = algdata_checks ~ml_type ~v ~c l;
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
      (* An output comes from C code, which allocated it: the stub can't be
         noalloc. The conversions themselves only copy the [value]. *)
      contract = Ty.copied ~ml_heap:true ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
      checks = Ty.no_checks;
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
  {
    routput;
    rc;
    rfree = Option.map bind' rty.cty.free;
    rc2ml = rty.contract.c2ml;
    rowned_by_ml = rty.contract.owned_by_ml;
    rcheck =
      List.map
        (fun (ch : check) ->
          { ch with cond = Expr.binds [ (rty.cty.c, e_var rc) ] ch.cond })
        rty.checks.c2ml_checks;
  }

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
                fmt "%a->t=Caml_ba_array_val(*%a)->data;@ " pp_var c' pp_var v');
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
    contract = Ty.owned_by_ml ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks = Ty.no_checks;
  }

let bigarray_array1 = bigarray_array1_aux

(* The OCaml value of [ty] is a block containing the OCaml value of [ty] and
   the OCaml value [parent], which is kept alive. *)
(* [ml2c] of [ty] applied to the first field of the OCaml value [v] *)
let ml2c_field0 (ty : mlc) ml2c v =
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr ty.cty.cty) in
  call_codef "ml2c_field"
    [ (v', e_addr v); (c', e_addr ty.cty.c) ]
    (fun { fmt } ->
      if not ty.contract.ml2c.alloc then
        fmt "value cid_temp = Field(*%a, 0);@," pp_var v'
      else (
        fmt "CAMLparam0();@,CAMLlocal1(cid_temp);@,";
        fmt "cid_temp = Field(*%a, 0);@," pp_var v');
      fmt "@[%a@]" pp_expr_binds
        (ml2c, ty_binds ~v:(expr "cid_temp") ~c:(expr "(*%a)" pp_var c') ty);
      if ty.contract.ml2c.alloc then fmt "@,CAMLreturn0;")

type child = {
  t : mlc;
  with_parent : ?c:var -> var -> mlc;
  parent_of : var -> expr;
  get_parent : string -> expr;
}

let keep_alive_with ~parent (ty : mlc) =
  let ml2c, c2ml = get_boxing ty.conv in
  let v = Var.mk "v" e_value in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr ty.cty.cty) in
  let parent' = Var.mk "parent" (expr "value *") in
  {
    ty with
    conv =
      Boxed
        {
          ml2c = ml2c_field0 ty ml2c v;
          c2ml =
            call_codef "c2ml"
              [
                (v', e_addr v); (c', e_addr ty.cty.c); (parent', e_addr parent);
              ]
              (fun { fmt } ->
                fmt "CAMLparam0 ();@,";
                fmt "CAMLlocal1(cid_temp);@,";
                fmt "@[%a@]@," pp_expr_binds
                  ( c2ml,
                    ty_binds ~v:(expr "cid_temp") ~c:(expr "(*%a)" pp_var c') ty
                  );
                fmt "*%a = caml_alloc_small(2, 0);@," pp_var v';
                fmt "Field(*%a, 0) = cid_temp;@," pp_var v';
                fmt "Field(*%a, 1) = *%a;@," pp_var v' pp_var parent';
                fmt "CAMLreturn0;");
        };
    v;
    contract =
      { ty.contract with c2ml = Ty.union ty.contract.c2ml Ty.allocates };
    checks =
      map_checks
        ~ml2c:(fun e -> Expr.binds [ (ty.v, expr "Field(%a, 0)" pp_var v) ] e)
        ty.checks;
  }

let keep_alive (ty : mlc) =
  let no_parent = Var.mk "no_parent" e_value in
  let t = keep_alive_with ~parent:no_parent ty in
  let no_c_parent _ =
    invalid_arg
      "Camlid: a child of keep_alive has no C parent, use Helper.parent and \
       Helper.child"
  in
  {
    t;
    with_parent = (fun ?c:_ parent -> keep_alive_with ~parent ty);
    parent_of = no_c_parent;
    get_parent = no_c_parent;
  }

type parent = {
  rc : defined;
  release : code;
  release_r : var;
  pmlty : expr;
  pops : defined;
}

(* The custom block contains a pointer to a structure with the C pointer
   [cty] and a reference counter, shared with the children. *)
(* Calls [init(first..., c, last...)] on a C value [c : cty]. With [malloc],
   [cty] is a pointer to [malloc], allocated before; otherwise [init] gets the
   address of [c], e.g. to set it to a pointer allocated by the C library. *)
let init_ptr ?malloc ?(first = []) ?(last = []) ~init ~cty ~c () =
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  call_codef "init"
    ((c', e_addr c) :: (first @ last))
    (fun { fmt } ->
      let pp_c fmt () =
        match malloc with
        | Some _ -> Fmt.pf fmt "*%a" pp_var c'
        | None -> pp_var fmt c'
      in
      let args =
        List.map (fun (v, _) fmt () -> pp_var fmt v) first
        @ [ pp_c ]
        @ List.map (fun (v, _) fmt () -> pp_var fmt v) last
      in
      Option.iter
        (fun ty -> fmt "*%a = malloc(sizeof(%a));@," pp_var c' pp_expr ty)
        malloc;
      fmt "%s(%a);" init Fmt.(list ~sep:comma (fun fmt pp -> pp fmt ())) args)

let parent ?malloc ?init ?clear ~ml ~cty () =
  let rc_id = ID.mk "rc" in
  let rc =
    toplevel ~kind:H rc_id
      "#include <stdatomic.h>@.@[<hv 2>typedef struct {@ %a ptr;@ _Atomic \
       intptr_t count;@ } %a;@]@."
      pp_expr cty pp_id rc_id
  in
  let r = Var.mk "r" (expr "%a *" pp_def rc) in
  let release =
    code "release"
      "@[if(atomic_fetch_sub(&%a->count, 1) == 1){@ %a%a@ free(%a);@ }@]" pp_var
      r
      Fmt.(option (fun fmt clear -> pf fmt "%s(%a->ptr);" clear pp_var r))
      clear
      Fmt.(option (fun fmt _ -> pf fmt "@ free(%a->ptr);" pp_var r))
      malloc pp_var r
  in
  let data v = expr "(*((%a **) Data_custom_val(%a)))" pp_def rc pp_var v in
  let finalize =
    let v = Var.mk "v" e_value in
    code "finalize_op" "%a" pp_calli (release, [ (r, data v) ])
  in
  let ops = custom_ops ~finalize () in
  let v = Var.mk "v" e_value in
  let c = Var.mk "c" cty in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  let t =
    {
      mlty = e_def @@ mlabstract ~keep_name:true ml;
      conv =
        Boxed
          {
            ml2c = expr "%a = %a->ptr;" pp_var c pp_expr (data v);
            c2ml =
              call_codef "c2ml"
                [ (v', e_addr v); (c', e_addr c) ]
                (fun { fmt } ->
                  fmt "%a *r = malloc(sizeof(%a));@," pp_def rc pp_def rc;
                  fmt "r->ptr = *%a;@," pp_var c';
                  fmt "atomic_init(&r->count, 1);@,";
                  fmt "*%a = caml_alloc_custom(&%a, sizeof(%a *), 0, 1);@,"
                    pp_var v' pp_def ops pp_def rc;
                  fmt "*((%a **) Data_custom_val(*%a)) = r;" pp_def rc pp_var v');
          };
      cty =
        {
          cty;
          init = Option.map (fun init -> init_ptr ?malloc ~init ~cty ~c ()) init;
          init_expr = expr "((%a) { 0 })" pp_expr cty;
          free = None;
          in_call = None;
          c;
        };
      v;
      contract = Ty.owned_by_ml ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
      checks = Ty.no_checks;
    }
  in
  (t, { rc; release; release_r = r; pmlty = t.mlty; pops = ops })

let child (parent : parent) ?malloc ?init ?(parent_last = false) ?clear
    ?clear_with_parent ~ml ~cty () =
  let st_id = ID.mk "child" in
  let st =
    toplevel ~kind:H st_id
      "@[<hv 2>typedef struct {@ %a ptr;@ %a *parent;@ } %a;@]@." pp_expr cty
      pp_def parent.rc pp_id st_id
  in
  let data v = expr "((%a *) Data_custom_val(%a))" pp_def st pp_var v in
  let data_star v = expr "((%a *) Data_custom_val(*%a))" pp_def st pp_var v in
  let finalize =
    let v = Var.mk "v" e_value in
    let s_var = Var.mk "s" (expr "%a *" pp_def st) in
    let release_r = parent.release_r in
    code ~locals:[ s_var ] "finalize_op" "%a *%a = %a;@ %a%a%a%a" pp_def st
      pp_var s_var pp_expr (data v)
      Fmt.(option (fun fmt clear -> pf fmt "%s(%a->ptr);@ " clear pp_var s_var))
      clear
      Fmt.(
        option (fun fmt clear ->
            if parent_last then
              pf fmt "%s(%a->ptr, %a->parent->ptr);@ " clear pp_var s_var pp_var
                s_var
            else
              pf fmt "%s(%a->parent->ptr, %a->ptr);@ " clear pp_var s_var pp_var
                s_var))
      clear_with_parent
      Fmt.(option (fun fmt _ -> pf fmt "free(%a->ptr);@ " pp_var s_var))
      malloc pp_calli
      (parent.release, [ (release_r, expr "%a->parent" pp_var s_var) ])
  in
  let ops = custom_ops ~finalize () in
  let v = Var.mk "v" e_value in
  let c = Var.mk "c" cty in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr cty) in
  let parent' = Var.mk "parent" (expr "value *") in
  let mlty = e_def @@ mlabstract ~keep_name:true ml in
  let mk ?init c2ml =
    {
      mlty;
      conv =
        Boxed { ml2c = expr "%a = %a->ptr;" pp_var c pp_expr (data v); c2ml };
      cty =
        {
          cty;
          init;
          init_expr = expr "((%a) { 0 })" pp_expr cty;
          free = None;
          in_call = None;
          c;
        };
      v;
      contract = Ty.owned_by_ml ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
      checks = Ty.no_checks;
    }
  in
  let with_parent ?c:parent_c parent_v =
    let init =
      match (init, parent_c) with
      | None, _ -> None
      | Some _, None ->
          invalid_arg
            "Camlid: the output of a child with init needs the C variable of \
             its parent: with_parent ~c"
      | Some init, Some parent_c ->
          let p = [ (Var.mk "parent" parent_c.ty, e_var parent_c) ] in
          Some
            (if parent_last then init_ptr ?malloc ~last:p ~init ~cty ~c ()
             else init_ptr ?malloc ~first:p ~init ~cty ~c ())
    in
    mk ?init
      (call_codef "c2ml"
         [ (v', e_addr v); (c', e_addr c); (parent', e_addr parent_v) ]
         (fun { fmt } ->
           fmt "%a *r = *((%a **) Data_custom_val(*%a));@," pp_def parent.rc
             pp_def parent.rc pp_var parent';
           fmt "atomic_fetch_add(&r->count, 1);@,";
           fmt "*%a = caml_alloc_custom(&%a, sizeof(%a), 0, 1);@," pp_var v'
             pp_def ops pp_def st;
           fmt "%a->ptr = *%a;@," pp_expr (data_star v') pp_var c';
           fmt "%a->parent = r;" pp_expr (data_star v')))
  in
  let t =
    mk
      (expr
         "#error(\"camlid: the output of a child needs the OCaml value of its \
          parent, use with_parent\")")
  in
  let parent_of v = expr "(%a->parent->ptr)" pp_expr (data v) in
  (* A new OCaml value for the parent, which shares its reference counter *)
  let get_parent mlname =
    let rc = Var.mk "c" (expr "%a *" pp_def parent.rc) in
    let pv = Var.mk "v" e_value in
    let rc' = Var.mk "c" (expr "%a **" pp_def parent.rc) in
    let pv' = Var.mk "v" (expr "value *") in
    let rty =
      {
        mlty = parent.pmlty;
        conv =
          Boxed
            {
              ml2c =
                expr
                  "#error(\"camlid: the parent of a child is only an output\")";
              c2ml =
                call_codef "c2ml_parent"
                  [ (pv', e_addr pv); (rc', e_addr rc) ]
                  (fun { fmt } ->
                    fmt "atomic_fetch_add(&(*%a)->count, 1);@," pp_var rc';
                    fmt "*%a = caml_alloc_custom(&%a, sizeof(%a *), 0, 1);@,"
                      pp_var pv' pp_def parent.pops pp_def parent.rc;
                    fmt "*((%a **) Data_custom_val(*%a)) = *%a;" pp_def
                      parent.rc pp_var pv' pp_var rc');
            };
        cty =
          {
            cty = expr "%a *" pp_def parent.rc;
            init = None;
            init_expr = expr "NULL";
            free = None;
            in_call = None;
            c = rc;
          };
        v = pv;
        contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
        checks = Ty.no_checks;
      }
    in
    let p, p_c, p_ml = param ~input:true ~name:"x" t in
    let result = simple_result rty in
    print_ml_fun ~mlname ~params:[ p ] ~call_params:[] ~result
      (expr "(void) %a;@ %a = %a->parent;" pp_var p_c pp_var result.rc pp_expr
         (data p_ml))
  in
  { t; with_parent; parent_of; get_parent }

(* An OCaml variant with constant constructors, and C constants *)
let enum_ml_type name constrs =
  let id = ID.mk ~keep_name:true name in
  toplevel ~kind:ML id "@[<hv 2>type %a =@ %a@]@." pp_id id
    Fmt.(list ~sep:sp (any "| " ++ string))
    (List.map fst constrs)

let enum ?(c = "int") name constrs =
  let mlty = enum_ml_type name constrs in
  let cty = expr "%s" c in
  let v = Var.mk "v" e_value in
  let cv = Var.mk "c" cty in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%s *" c) in
  let constrs = List.mapi (fun i (_, cst) -> (i, cst)) constrs in
  {
    mlty = e_def mlty;
    conv =
      Boxed
        {
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr cv) ]
              (fun { fmt } ->
                fmt "@[<v 2>switch(Long_val(*%a)){@," pp_var v';
                List.iter
                  (fun (i, cst) ->
                    fmt "case %i: *%a = %s; break;@," i pp_var c' cst)
                  constrs;
                fmt "@]}");
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr cv) ]
              (fun { fmt } ->
                fmt "@[<v 2>switch(*%a){@," pp_var c';
                List.iter
                  (fun (i, cst) ->
                    fmt "case %s: *%a = Val_long(%i); break;@," cst pp_var v' i)
                  constrs;
                fmt "default: *%a = Val_long(0);@]@,}" pp_var v');
        };
    cty =
      {
        cty;
        init = None;
        init_expr = expr "((%s) 0)" c;
        free = None;
        in_call = None;
        c = cv;
      };
    v;
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.noalloc ();
    checks =
      {
        ml2c_checks = [];
        c2ml_checks =
          [
            {
              cond =
                expr "!(%a)"
                  Fmt.(
                    list ~sep:(any " || ") (fun fmt (_, cst) ->
                        pf fmt "%a == %s" pp_var cv cst))
                  constrs;
              exn = Check_failure;
              msg = "unknown value of " ^ name;
            };
          ];
      };
  }

let flags ?(c = "int") name constrs =
  let mlty = enum_ml_type name constrs in
  let cty = expr "%s" c in
  let v = Var.mk "v" e_value in
  let cv = Var.mk "c" cty in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%s *" c) in
  let csts = List.map snd constrs in
  {
    mlty = expr "%a list" pp_def mlty;
    conv =
      Boxed
        {
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr cv) ]
              (fun { fmt } ->
                fmt "@[static const %s cid_table[] = { %a };@]@," c
                  Fmt.(list ~sep:comma string)
                  csts;
                fmt "*%a = 0;@," pp_var c';
                fmt
                  "@[<v 2>for(value cid_l = *%a; cid_l != Val_emptylist; cid_l \
                   = Field(cid_l, 1)){@,"
                  pp_var v';
                fmt "*%a |= cid_table[Long_val(Field(cid_l, 0))];@]@,}" pp_var
                  c');
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr cv) ]
              (fun { fmt } ->
                fmt "@[static const %s cid_table[] = { %a };@]@," c
                  Fmt.(list ~sep:comma string)
                  csts;
                fmt "CAMLparam0();@,CAMLlocal1(cid_cell);@,";
                fmt "*%a = Val_emptylist;@," pp_var v';
                fmt
                  "@[<v 2>for(intptr_t cid_i = %i; cid_i >= 0; cid_i--){@,\
                   @[<v 2>if((*%a & cid_table[cid_i]) == cid_table[cid_i]){@,\
                   cid_cell = caml_alloc_small(2, 0);@,\
                   Field(cid_cell, 0) = Val_long(cid_i);@,\
                   Field(cid_cell, 1) = *%a;@,\
                   *%a = cid_cell;@]@,\
                   }@]@,\
                   }@,"
                  (List.length csts - 1)
                  pp_var c' pp_var v' pp_var v';
                fmt "CAMLreturn0;");
        };
    cty =
      {
        cty;
        init = None;
        init_expr = expr "((%s) 0)" c;
        free = None;
        in_call = None;
        c = cv;
      };
    v;
    contract = Ty.copied ~ml2c:Ty.noalloc ~c2ml:Ty.allocates ();
    checks = Ty.no_checks;
  }

(* [None] is the NULL pointer *)
let nullable (ty : mlc) =
  let ml2c, c2ml =
    match ty.conv with
    | Boxed { ml2c; c2ml } -> (ml2c, c2ml)
    | Unboxable _ -> invalid_arg "Camlid.nullable: the type is not a pointer"
  in
  let v = Var.mk "v" e_value in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_expr ty.cty.cty) in
  {
    mlty = expr "(%a option)" pp_expr ty.mlty;
    conv =
      Boxed
        {
          ml2c =
            expr "@[if(Is_long(%a)) %a = NULL;@ else {@ %a@ }@]" pp_var v pp_var
              ty.cty.c pp_expr (ml2c_field0 ty ml2c v);
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr ty.cty.c) ]
              (fun { fmt } ->
                fmt "@[<v 2>if(*%a == NULL) *%a = Val_int(0);@]@," pp_var c'
                  pp_var v';
                fmt "@[<v 2>else {@,CAMLparam0();@,CAMLlocal1(cid_temp);@,";
                fmt "@[%a@]@," pp_expr_binds
                  ( c2ml,
                    ty_binds ~v:(expr "cid_temp") ~c:(expr "(*%a)" pp_var c') ty
                  );
                fmt "*%a = caml_alloc_small(1, 0);@," pp_var v';
                fmt "Field(*%a, 0) = cid_temp;@," pp_var v';
                fmt "CAMLreturn0;@]@,}");
        };
    cty =
      {
        ty.cty with
        free =
          Option.map
            (fun free ->
              expr "@[if(%a != NULL){@ %a@ }@]" pp_var ty.cty.c pp_expr free)
            ty.cty.free;
      };
    v;
    contract =
      { ty.contract with c2ml = Ty.union ty.contract.c2ml Ty.allocates };
    checks =
      map_checks
        ~ml2c:(fun e ->
          expr "(Is_block(%a) && (%a))" pp_var v pp_expr
            (Expr.binds [ (ty.v, expr "Field(%a, 0)" pp_var v) ] e))
        ~c2ml:(fun e -> expr "(%a != NULL && (%a))" pp_var ty.cty.c pp_expr e)
        ty.checks;
  }
