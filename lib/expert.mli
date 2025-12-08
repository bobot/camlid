val typedef : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
val mlalias : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
val mlabstract : ?keep_name:bool -> string -> Expr.defined
val existing : string -> Expr.var list -> Expr.code
val calli_existing : string -> Expr.var list -> Expr.expr
val get_boxing : Type.conv -> Expr.expr * Expr.expr

val builtin_mltypes :
  unbox_attribute:Type.unbox_attribute ->
  unbox_version:int * int ->
  ?u_type:string ->
  c_type:string ->
  c2ml:string ->
  ml2c:string ->
  ?u2c:string ->
  ?c2u:string ->
  ?ml2u:string ->
  ?u2ml:string ->
  string ->
  Type.mlc

val string_nt : ?owned:bool -> unit -> Type.mlc
val string_fixed_length : ?init:bool -> ?owned:bool -> Expr.var -> Type.mlc
val string_length_struct : Expr.defined
val string_length : ?owned:bool -> unit -> Type.mlc
val ptr_ref : Type.mlc -> Type.mlc

type copy = { copy : Expr.expr; c_from : Expr.Var.t; c_to : Expr.Var.t }

val mk_copy :
  cty:Expr.expr ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  copy

val copy : copy:copy -> Type.mlc -> Type.mlc
val array : ?init:bool -> ?owned:bool -> len:Expr.var -> Type.mlc -> Type.mlc
val array_length : ?owned:bool -> Type.mlc -> Type.mlc

val map_param_in_call :
  ?name:string ->
  (Expr.expr -> Expr.expr -> Expr.expr * Expr.expr) ->
  Type.param ->
  Type.param

val deref_in_call : Type.param -> Type.param
val use_new_param_only_in_call : Type.param -> Type.param
val get_field : Expr.expr -> string -> Type.param -> Type.param
val t_field : Type.mlc -> Type.param -> Type.param
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
  Type.mlc

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
  Type.mlc

val custom_ptr :
  ?initialize:initialize ->
  ?finalize:finalize ->
  ?hash:hash ->
  ?compare:compare ->
  ?malloc:bool ->
  ml:string ->
  cty:Expr.expr ->
  unit ->
  Type.mlc

val e_value : Expr.expr
val value : string -> Type.mlc

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
  Type.mlc ->
  Type.param * Expr.var

val simple_result : Type.mlc -> Type.result

val list_or_empty :
  empty:(Format.formatter -> unit -> unit) ->
  sep:unit Fmt.t ->
  'a Fmt.t ->
  Format.formatter ->
  'a list ->
  unit

val code_c_fun :
  params:Type.param list ->
  call_params:Type.param list ->
  result:Type.result option ->
  name:string ->
  Expr.expr ->
  Expr.code
(** @param call_params
      should be a reordered subset of params (usually it is [params]) *)

val code_c_fun_bytecode :
  params:Type.param list -> result:Type.result option -> Expr.code -> Expr.code

val print_ml_fun :
  params:Type.param list ->
  ?call_params:Type.param list ->
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
  mlc:Type.mlc ->
  c:Type.c ->
  unit ->
  Type.mlc

module AlgData : sig
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * Type.mlc) list

  type constr = {
    name : string;
    tag : Expr.id;
    smart_constructor : Expr.code;
    kind : kind;
  }

  type t = {
    ty : Type.mlc;
    constrs : constr list;
    dst_smart_constructors : Expr.var;
  }

  val algdata : string -> (string * (string * Type.mlc) list) list -> t
end

val ret_option_if : Expr.expr -> Type.mlc -> Type.mlc
(** [ret_option_if expr ty] the ocaml calue returned is [Some v] if the C
    expression [expr] is true, and [v] is returned by [ty], otherwise it is
    [None]*)

val get_expression : mlname:string -> Type.mlc -> Expr.expr -> Expr.expr
(** [get_expression ~mlname ty expr] defined the function [mlname] that return
    the value of the C expression [expr] of type [ty] *)

val bigarray_array1 :
  managed:string ->
  kind:string ->
  cty:string ->
  mlty:string ->
  mlelt:string ->
  unit ->
  Type.mlc
(** The biggaray is mirrored by a structure with a field "len" of type size_t
    and a field "t" of type "cty*"
    @param managed C constant name for the managed flag
    @param kind C constant name for the kind flag
    @param cty the C type of the C elements
    @param mlty the OCaml type of the OCaml elements
    @param mlelt the OCaml type witness for the C elements *)
