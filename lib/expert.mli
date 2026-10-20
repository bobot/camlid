(** Building new types and parameters.

    A type ({!Ty.mlc}) is a template: its fields mention the variables [v] (the
    OCaml value) and [cty.c] (the C value), which are replaced by fresh
    variables each time the type is used in a parameter. Every type must follow
    the memory model described in {!page-index.memory}. *)

(** {1 C and OCaml type names} *)

val typedef : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
(** [typedef name "fmt" ...] defines a C type [typedef ... prefix_name]. *)

val mlalias : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a
(** [mlalias name "fmt" ...] defines an OCaml type [type name = ...]. *)

val mlabstract : ?keep_name:bool -> string -> Expr.defined
(** [mlabstract name] defines an abstract OCaml type [type name]. *)

val declare_struct : string -> (string * Expr.expr) list -> Expr.defined
(** [declare_struct name fields] defines a C structure. *)

(** {1 Types} *)

val builtin_mltypes :
  unbox_attribute:Ty.unbox_attribute ->
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
  Ty.mlc
(** Scalar types, unboxed or untagged in native code from OCaml [unbox_version].
    The arguments are names of C conversion functions or macros. *)

val value : string -> Ty.mlc
(** [value ty] is the OCaml type [ty], given as a [value] to the C function. *)

val array : ?init:bool -> ?owned:bool -> len:Expr.var -> Ty.mlc -> Ty.mlc
(** [array ~len ty] is an OCaml array of [ty] and a C array of [len] elements.
    @param init allocate (and initialize the elements) when it is not an input
*)

val string_fixed_length : ?init:bool -> ?owned:bool -> Expr.var -> Ty.mlc
(** [string_fixed_length len] is an OCaml [string] and a C [char *] of [len]
    bytes.
    @param init allocate when it is not an input *)

(** {2 Values owned by OCaml}

    The C value is stored in an OCaml block ([owned_by_ml] is [true]). [icty] is
    the C type stored in the block and [cty] the C type given to the stubbed
    functions (usually the same). The record arguments are created with the
    [mk_*] functions below. *)

type init = { init : Expr.expr; c : Expr.var }
(** Initialize [c : cty *], used when the type is not an input *)

type get = { get : Expr.expr; c : Expr.var; i : Expr.var }
(** Get [c : cty *] from [i : icty *] *)

type set = { set : Expr.expr; c : Expr.var; i : Expr.var }
(** Set [i : icty *] from [c : cty *] *)

type clear = { clear : Expr.expr; i : Expr.var }
(** Called on [i : icty *] when the OCaml value is collected *)

type hash = { hash : Expr.expr; i : Expr.var }
(** Hash of [i : icty *] *)

type compare = { compare : Expr.expr; i1 : Expr.var; i2 : Expr.var }
(** Comparison of [i1 : icty *] and [i2 : icty *] *)

type mem = { mem : Expr.expr; c : Expr.var }
(** Size in bytes of the memory outside the OCaml heap owned by [c : cty *],
    given to the GC with [caml_alloc_custom_mem] *)

val abstract :
  ?init:init ->
  ?get:get ->
  ?set:set ->
  icty:Expr.expr ->
  ml:string ->
  cty:Expr.expr ->
  unit ->
  Ty.mlc
(** The C value is copied in an [Abstract_tag] block, it can't be finalized. *)

val custom :
  ?init:init ->
  ?clear:clear ->
  ?hash:hash ->
  ?compare:compare ->
  ?mem:mem ->
  ?get:get ->
  ?set:set ->
  ml:string ->
  icty:Expr.expr ->
  cty:Expr.expr ->
  unit ->
  Ty.mlc
(** The C value is moved in a custom block. *)

val custom_ptr :
  ?init:init ->
  ?clear:clear ->
  ?hash:hash ->
  ?compare:compare ->
  ?mem:mem ->
  ?dispose:string ->
  ?malloc:bool ->
  ml:string ->
  cty:Expr.expr ->
  unit ->
  Ty.mlc
(** The custom block contains a pointer, the stubbed functions get a [cty *].
    The record arguments are created with [~icty:cty].
    @param malloc
      (default [true]) allocate with [malloc] before [init], and [free] after
      [clear]. The size of the allocation is given to the GC with [mem].
    @param dispose
      name of an OCaml function [dispose : t -> unit] added to the OCaml file,
      which clears the value before it is collected. A disposed value raises
      [Invalid_argument] when it is used. *)

(** The [mk_*] functions build the records above for the C function [name]. By
    default it is called with the variables of the record, [vars] allows to add
    other variables (e.g. the C variable of another parameter), and [exprs] to
    give any expressions. *)

val mk_init :
  cty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  init

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

val mk_clear :
  icty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  clear

val mk_hash :
  icty:Expr.expr ->
  ?vars:(Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(Expr.Var.t -> Expr.expr list) ->
  string ->
  hash

val mk_compare : icty:Expr.expr -> string -> compare

val mk_mem : cty:Expr.expr -> string -> mem
(** [mk_mem ~cty f] calls [size_t f(cty *c)] *)

(** {2 Parents and children}

    A C value (the child) can depend on another one (the parent), e.g. a
    statement on its database. *)

type child = {
  t : Ty.mlc;  (** for the inputs *)
  with_parent : Expr.var -> Ty.mlc;
      (** [with_parent p] for the outputs, where [p] is the OCaml variable of
          the parent input, e.g. returned by {!param} *)
}

val keep_alive : Ty.mlc -> child
(** [keep_alive ty] is [ty] where the OCaml value of an output also keeps alive
    the OCaml value of the parent. Use it when the parent must stay alive while
    the child is used, but is not needed to destroy the child: the finalizers of
    two values collected together run in any order. *)

type parent
(** A C pointer shared by the parent and its children with a reference counter
*)

val parent :
  ?clear:string -> ml:string -> cty:Expr.expr -> unit -> Ty.mlc * parent
(** [parent ~clear ~ml ~cty ()] is a pointer [cty] allocated by the C library.
    [void clear(cty)] is called when the OCaml value and all the children are
    collected. *)

val child :
  parent ->
  ?clear:string ->
  ?clear_with_parent:string ->
  ml:string ->
  cty:Expr.expr ->
  unit ->
  child
(** [child parent ~ml ~cty ()] is a pointer [cty] allocated by the C library,
    which keeps a reference on its parent. When it is collected
    [void clear(cty)] or [void clear_with_parent(parent_cty, cty)] is called,
    then the reference is released. *)

(** {2 Copies and conversions} *)

type copy = { copy : Expr.expr; c_from : Expr.Var.t; c_to : Expr.Var.t }
(** Copy from [c_from : cty *] to [c_to : cty *] *)

val mk_copy :
  cty:Expr.expr ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  copy

val copy : copy:copy -> Ty.mlc -> Ty.mlc
(** [copy ~copy ty] is [ty] where the C value of an output is owned by C: it is
    copied into a new C value, which is moved into the OCaml value. *)

type convert = { convert : Expr.expr; src : Expr.var; dst : Expr.var }
(** Conversion from [src : src_type *] to [dst : dst_type *] *)

val mk_converter :
  src:Ty.c ->
  dst:Ty.c ->
  ?vars:(dst:Expr.var -> src:Expr.var -> Expr.var list) ->
  ?exprs:(dst:Expr.var -> src:Expr.var -> Expr.expr list) ->
  string ->
  convert

val convert :
  ?mlc_to_c:convert ->
  ?c_to_mlc:convert ->
  mlc:Ty.mlc ->
  c:Ty.c ->
  unit ->
  Ty.mlc
(** [convert ~mlc ~c] has the OCaml type of [mlc] and the C type [c]. The
    conversions go through a temporary C value of [mlc]; the destination is
    initialized before the conversion and the temporary is freed after. *)

(** {2 Other types} *)

module AlgData : sig
  type kind =
    | KConst of int
    | KNonConst of int * (string * Expr.var * Ty.mlc) list

  type constr = {
    name : string;
    tag : Expr.id;
    smart_constructor : Expr.code;
    kind : kind;
  }

  type t = {
    ty : Ty.mlc;
    constrs : constr list;
    dst_smart_constructors : Expr.var;
  }

  val algdata : string -> (string * (string * Ty.mlc) list) list -> t
end

val ret_option_if : Expr.expr -> Ty.mlc -> Ty.mlc
(** [ret_option_if expr ty] the OCaml value returned is [Some v] if the C
    expression [expr] is true, where [v] is returned by [ty], otherwise it is
    [None] *)

val bigarray_array1 :
  managed:string ->
  kind:string ->
  cty:string ->
  mlty:string ->
  mlelt:string ->
  unit ->
  Ty.mlc
(** The bigarray is mirrored by a structure with a field "len" of type size_t
    and a field "t" of type "cty*"
    @param managed C constant name for the managed flag
    @param kind C constant name for the kind flag
    @param cty the C type of the C elements
    @param mlty the OCaml type of the OCaml elements
    @param mlelt the OCaml type witness for the C elements *)

(** {1 Parameters} *)

val param :
  ?input_label:string ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?input:bool ->
  ?output:bool ->
  ?used_in_call:bool ->
  ?name:string ->
  Ty.mlc ->
  Ty.param * Expr.var * Expr.var
(** [param ty] creates a parameter from the template [ty] with fresh variables.
    It returns the parameter, its C variable and its OCaml variable. They can be
    used in the definition of the other parameters of the same function.

    Raises [Invalid_argument] for an input and output of a type with
    [owned_by_ml]. *)

val simple_param :
  ?input_label:string ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?input:bool ->
  ?output:bool ->
  ?used_in_call:bool ->
  ?name:string ->
  Ty.mlc ->
  Ty.param * Expr.var
(** Same as {!param} without the OCaml variable. *)

val map_param_in_call :
  ?name:string ->
  (Expr.expr -> Expr.expr -> Expr.expr * Expr.expr) ->
  Ty.param ->
  Ty.param
(** [map_param_in_call f p] changes the type and the expression given to the
    stubbed function. *)

val deref_in_call : Ty.param -> Ty.param
(** Give the address of the C variable to the stubbed function. *)

(** {1 Errors} *)

type exn_
(** An OCaml exception with a string argument, raised by the stubs *)

val declare_exception : ?registered:string -> string -> exn_
(** [declare_exception name] declares [exception name of string] in the OCaml
    file, registered with [Callback.register_exception].
    @param registered
      the registered name (default [name]), it should be unique among the
      libraries, e.g. ["Mylib.Error"] *)

type error = {
  cond : Expr.expr;  (** C condition, true when the C function failed *)
  msg : Expr.expr option;
      (** C string, the argument of the exception (default ["f failed"]) *)
  exn : exn_ option;  (** default [Failure] *)
}
(** The check done after the call. On failure, the outputs owned by OCaml that
    were initialized are moved into OCaml values (which will be finalized), the
    other values are freed, and the exception is raised. *)

val get_expression : mlname:string -> Ty.mlc -> Expr.expr -> Expr.expr
(** [get_expression ~mlname ty expr] defines the function [mlname] that returns
    the value of the C expression [expr] of type [ty] *)

(**/**)

(* Used by Helper *)

val existing : string -> Expr.var list -> Expr.code
val calli_existing : string -> Expr.var list -> Expr.expr
val get_boxing : Ty.conv -> Expr.expr * Expr.expr
val e_value : Expr.expr
val string_nt : ?owned:bool -> unit -> Ty.mlc
val string_nt_prefix : ?owned:bool -> unit -> Ty.mlc
val string_length_struct : Expr.defined
val string_length : ?owned:bool -> unit -> Ty.mlc
val ptr_ref : Ty.mlc -> Ty.mlc
val array_length : ?owned:bool -> Ty.mlc -> Ty.mlc
val use_new_param_only_in_call : Ty.param -> Ty.param
val get_field : Expr.expr -> string -> Ty.param -> Ty.param
val t_field : Ty.mlc -> Ty.param -> Ty.param
val len_field : Ty.param -> Ty.param
val simple_result : Ty.mlc -> Ty.result

val list_or_empty :
  empty:(Format.formatter -> unit -> unit) ->
  sep:unit Fmt.t ->
  'a Fmt.t ->
  Format.formatter ->
  'a list ->
  unit

val code_c_fun :
  ?noalloc:bool ->
  ?release_lock:bool ->
  ?error:error ->
  params:Ty.param list ->
  call_params:Ty.param list ->
  result:Ty.result option ->
  name:string ->
  Expr.expr ->
  Expr.code
(** @param call_params
      should be a reordered subset of params (usually it is [params])
    @param release_lock
      release the runtime lock during the call; raises [Invalid_argument] if an
      argument points into the OCaml heap *)

val code_c_fun_bytecode :
  params:Ty.param list -> result:Ty.result option -> Expr.code -> Expr.code

val print_ml_fun :
  ?noalloc:bool ->
  ?release_lock:bool ->
  ?error:error ->
  params:Ty.param list ->
  ?call_params:Ty.param list ->
  ?result:Ty.result ->
  mlname:string ->
  Expr.expr ->
  Expr.expr

val if_ : ?else_:Expr.expr -> Expr.expr -> then_:Expr.expr -> Expr.expr
val seq : Expr.expr list -> Expr.expr
