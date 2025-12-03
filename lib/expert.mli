val typedef : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
val mlalias : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
val mlabstract : ?keep_name:bool -> string -> Expr.defined
val existing : string -> Expr.var list -> Expr.code
val calli_existing : string -> Expr.var list -> Expr.expr
val get_boxing : Type.conv -> Expr.expr * Expr.expr

val builtin_mltypes :
  unbox_attribute:Type.unbox_attribute ->
  ?u_type:string ->
  c_type:string ->
  c2ml:string ->
  ml2c:string ->
  ?u2c:string ->
  ?c2u:string ->
  ?ml2u:string ->
  ?u2ml:string ->
  string ->
  Type.typedef

val string_nt : ?owned:bool -> unit -> Type.typedef
val string_fixed_length : ?init:bool -> ?owned:bool -> Expr.var -> Type.typedef
val string_length_struct : Expr.defined
val string_length : ?owned:bool -> unit -> Type.typedef
val ptr_ref : Type.typedef -> Type.typedef

type copy = { copy : Expr.expr; c_from : Expr.Var.t; c_to : Expr.Var.t }

val mk_copy :
  cty:Expr.expr ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  copy

val copy : copy:copy -> Type.typedef -> Type.typedef

val array :
  ?init:bool -> ?owned:bool -> len:Expr.var -> Type.typedef -> Type.typedef

val array_length : ?owned:bool -> Type.typedef -> Type.typedef

val map_param_in_call :
  ?name:string ->
  (Expr.expr -> Expr.expr -> Expr.expr * Expr.expr) ->
  Type.param ->
  Type.param

val deref_in_call : Type.param -> Type.param
val use_new_param_only_in_call : Type.param -> Type.param
val get_field : Expr.expr -> string -> Type.param -> Type.param
val t_field : Type.typedef -> Type.param -> Type.param
val len_field : Type.param -> Type.param

type get = { get : Expr.expr; c : Expr.var; i : Expr.var }
type set = { set : Expr.expr; c : Expr.var; i : Expr.var }
type initialize = { initialize : Expr.expr; c : Expr.var }

val abstract :
  ?initialize:initialize ->
  ?get:get ->
  ?set:set ->
  icty:Expr.defined ->
  ml:string ->
  cty:Expr.defined ->
  unit ->
  Type.typedef

type finalize = { finalize : Expr.expr; i : Expr.var }
type finalize_op = { finalize_op : Expr.code; v : Expr.var }
type hash = { hash : Expr.expr; i : Expr.var }
type hash_op = { hash_op : Expr.code; v : Expr.var }
type compare = { compare : Expr.expr; i1 : Expr.var; i2 : Expr.var }
type compare_op = { compare_op : Expr.code; v1 : Expr.var; v2 : Expr.var }

val custom :
  ?initialize:initialize ->
  ?finalize:finalize ->
  ?hash:hash ->
  ?compare:compare ->
  ?get:get ->
  ?set:set ->
  ml:string ->
  icty:Expr.expr ->
  cty:Expr.expr ->
  unit ->
  Type.typedef

val custom_ptr :
  ?initialize:initialize ->
  ?finalize:finalize ->
  ?hash:hash ->
  ?compare:compare ->
  ?malloc:bool ->
  ml:string ->
  cty:Expr.expr ->
  unit ->
  Type.typedef

val e_value : Expr.expr
val value : string -> Type.typedef

val mk_get :
  icty:Expr.expr ->
  cty:Expr.expr ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  get

val mk_set :
  icty:Expr.expr ->
  cty:Expr.expr ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  set

val mk_finalize :
  icty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  finalize

val mk_finalize_ptr :
  icty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  finalize

val mk_hash :
  icty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  hash

val mk_compare : icty:Expr.expr -> string -> compare

val mk_initialize :
  cty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  initialize

val simple_param :
  ?input_label:string ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?input:bool ->
  ?output:bool ->
  ?used_in_call:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Expr.var

val simple_result : Type.typedef -> Type.result

val list_or_empty :
  empty:(Format.formatter -> unit -> unit) ->
  sep:unit Fmt.t ->
  'a Fmt.t ->
  Format.formatter ->
  'a list ->
  unit

val code_c_fun :
  params:Type.param list ->
  result:Type.result option ->
  name:string ->
  Expr.expr ->
  Expr.code

val code_c_fun_bytecode :
  params:Type.param list -> result:Type.result option -> Expr.code -> Expr.code

val print_ml_fun :
  params:Type.param list ->
  ?result:Type.result ->
  mlname:string ->
  Expr.expr ->
  Expr.expr

val declare_struct : string -> (string * Expr.expr) list -> Expr.defined
val if_ : ?else_:Expr.expr -> Expr.expr -> then_:Expr.expr -> Expr.expr
val seq : Expr.expr list -> Expr.expr

type convert = { convert : Expr.expr; src : Expr.var; dst : Expr.var }

val mk_converter :
  src:Type.c ->
  dst:Type.c ->
  ?vars:(dst:Expr.var -> src:Expr.var -> Expr.var list) ->
  ?exprs:(dst:Expr.var -> src:Expr.var -> Expr.expr list) ->
  string ->
  convert

val convert :
  ?mlc_to_c:convert ->
  ?c_to_mlc:convert ->
  mlc:Type.typedef ->
  c:Type.c ->
  unit ->
  Type.typedef

module AlgData : sig
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * Type.typedef) list

  type constr = {
    name : string;
    tag : Expr.id;
    smart_constructor : Expr.code;
    kind : kind;
  }

  type t = {
    ty : Type.typedef;
    constrs : constr list;
    dst_smart_constructors : Expr.var;
  }

  val algdata : string -> (string * (string * Type.typedef) list) list -> t
end

val ret_option_if : Expr.expr -> Type.typedef -> Type.typedef
val get_expression : mlname:string -> Type.typedef -> Expr.expr -> Expr.expr
