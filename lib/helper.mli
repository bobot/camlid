(** Description of C functions with the common types.

    The memory handled by each type is described in {!page-index.memory}. *)

(** {1 Functions} *)

type error
(** A check done after the call of the C function, see {!val:error} *)

val func :
  ?noalloc:bool ->
  ?release_lock:bool ->
  ?error:error ->
  ?ml:string ->
  ?result:Ty.mlc ->
  ?ignored_result:Ty.mlc ->
  ?call_params:Ty.param list ->
  string ->
  Ty.param list ->
  Expr.expr
(** [func c_name params] is the OCaml external for the C function [c_name]. Its
    stub is added to the C file by {!Generate.to_file}.

    The order of the inputs, of the outputs (result first), and of the arguments
    of the C function is the order of [params].

    @param ml name of the OCaml function (default [c_name])
    @param result the C function returns a value, part of the OCaml results
    @param ignored_result the C function returns a value which is ignored
    @param call_params
      the parameters in the order of the C function arguments, when it is not
      the order of [params]
    @param release_lock
      release the runtime lock during the call, so that other threads can run.
      Raises [Invalid_argument] if an argument of the C function points into the
      OCaml heap (e.g. {!input_value}).
    @param error checks the result of the C function, see {!val:error}
    @param noalloc
      by default the external is [[@@noalloc]] when the conversions don't
      allocate in the OCaml heap (e.g. scalars and inputs owned by OCaml), and
      there is at most one result, no [error] and no [release_lock]. Use
      [~noalloc:false] when the C function itself uses the OCaml runtime (e.g.
      raises an exception). [~noalloc:true] fails if it is not possible. *)

(** {2 Errors} *)

val exception_ : ?registered:string -> string -> Expert.exn_
(** [exception_ name] declares in the OCaml file [exception name of string],
    registered under the name [registered] (default [name]) which should be
    unique among the libraries, e.g. ["Mylib.Error"]. *)

val error :
  ?exn:Expert.exn_ -> ?msg:string -> ?vars:Expr.var list -> string -> error
(** [error is_error] raises an exception when the C function
    [int is_error(res, vars...)] returns true, where [res] is the result of the
    C function (given by [result] or [ignored_result]), if any. Before raising,
    the C values are freed as usual, and the outputs owned by OCaml are
    finalized.
    @param msg
      the C function [const char * msg(res, vars...)] gives the argument of the
      exception (default ["f failed"])
    @param exn the exception (default [Failure]) *)

(** The following functions create a parameter from a type with fresh variables.
    A parameter can be used in many functions but only once by function.

    {t
      |                | OCaml argument | C argument | OCaml result |
      |----------------|:--------------:|:----------:|:------------:|
      | {!input}       |       x        |     x      |              |
      | {!output}      |                |     x      |      x       |
      | {!inout}       |       x        |     x      |      x       |
      | {!ignored}     |                |     x      |              |
      | {!extra_input} |       x        |            |              |
    } *)

val input : ?name:string -> Ty.mlc -> Ty.param
val output : ?name:string -> Ty.mlc -> Ty.param

val inout : ?name:string -> Ty.mlc -> Ty.param
(** Raises [Invalid_argument] for types owned by OCaml. *)

val ignored : ?name:string -> Ty.mlc -> Ty.param

val extra_input : ?label:string -> ?name:string -> Ty.mlc -> Ty.param * Expr.var
(** [extra_input ty] returns the parameter and its C variable, which can be used
    by the other parameters, e.g. in the [init_args] of {!val:value_type}.
    @param label label of the argument in the OCaml function *)

(** {1 Scalars}

    No memory is allocated. They are unboxed or untagged when possible. *)

val int : Ty.mlc
(** [int] in OCaml, [intptr_t] in C. The highest bit is lost from C to OCaml. *)

val size_t : Ty.mlc
(** [int] in OCaml, [size_t] in C. The highest bit is lost from C to OCaml. *)

val c_int : Ty.mlc
(** [int] in OCaml, [int] in C. On 64 bits, the 31 highest bits are lost from
    OCaml to C. *)

val double : Ty.mlc
(** [float] in OCaml, [double] in C. *)

val int32 : Ty.mlc
(** [int32] in OCaml, [int32_t] in C. *)

val int64 : Ty.mlc
(** [int64] in OCaml, [int64_t] in C. *)

val nativeint : Ty.mlc
(** [nativeint] in OCaml, [intptr_t] in C. *)

val bool : Ty.mlc
(** [bool] in OCaml, [int] in C. *)

val ptr_ref : Ty.mlc -> Ty.mlc
(** [ptr_ref ty] gives to the C function a pointer to the C value of [ty],
    typically for an {!output}. The OCaml type is the one of [ty]. *)

(** {1 Strings and arrays}

    The data is copied between OCaml and C. [owned] (default [true]) indicates
    that the stub frees the C memory after the call. Use [~owned:false] when the
    C function takes ownership of an input, or keeps ownership of an output.
    [init] (default [true]) indicates that the stub allocates the memory of an
    output before the call.

    The functions returning {!with_length} are named after how the C function
    receives the data: [_len] for a pointer and a length, [_len_ref] for
    pointers to both (e.g. filled by the C function), [fixed_length_] for a
    buffer allocated by the stub. [input] and [output] tell if the data is an
    argument or a result of the OCaml function. *)

val string_nt : ?owned:bool -> unit -> Ty.mlc
(** [string] in OCaml, null terminated [char *] in C. The conversion to C raises
    [Failure] if the OCaml string contains a null character. *)

val string_nt_prefix : ?owned:bool -> unit -> Ty.mlc
(** Same as {!string_nt} but the conversion to C silently stops at the first
    null character. *)

type with_length = {
  t : Ty.param;  (** the data *)
  len : Ty.param;  (** its length, only usable with [t] in the same function *)
}

val string_len :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
(** A [string] given to C as [char *] and [size_t], by default an input. *)

val string_len_ref :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
(** A [string] given to C as [char **] and [size_t *], by default an output. *)

val fixed_length_string :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  unit ->
  with_length
(** A [string] given to C as [char *], allocated by the stub with the length
    given by the OCaml argument [len].
    @param len_used_in_call also give the length to the C function *)

val array_len :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [array_len ty] is an array of [ty] given to C as [ty *] and [size_t], by
    default an input. *)

val array_len_ref :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [array_len_ref ty] is an array of [ty] given to C as [ty **] and [size_t *],
    by default an output. *)

val fixed_length_array :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [fixed_length_array ty] is an array of [ty] given to C as [ty *], allocated
    (and its elements initialized) by the stub with the length given by the
    OCaml argument [len].
    @param len_used_in_call also give the length to the C function *)

(** {1 C values owned by OCaml} *)

type value_type = {
  t : Ty.mlc;
      (** [T *] in C. The OCaml value is a custom block that owns a [T]. An
          input borrows it, an output is created with [init] and moved into a
          new OCaml value. *)
  value : Ty.mlc;
      (** [T] in C, e.g. for the elements of arrays. It is copied with [copy]
          from or to a new [T] owned by OCaml, and cleared at the end of the
          stub. *)
  copy_of_ptr : Ty.mlc;
      (** [T *] in C for an output owned by C, e.g. a pointer inside another
          structure. It is copied into a new [T] owned by OCaml. *)
}
(** The same OCaml type for the three ways a C function can use a [T]. *)

val value_type :
  ?init_args:Expr.var list ->
  ?hash:string ->
  ?compare:string ->
  ?mem:string ->
  ?dispose:string ->
  init:string ->
  clear:string ->
  copy:string ->
  ml:string ->
  c:string ->
  unit ->
  value_type
(** [value_type ~init ~clear ~copy ~ml ~c ()] binds the C type [c] (named [T]
    below) handled like a C++ class to the abstract OCaml type [ml]. The C
    functions are:
    - [void init(T *t, init_args...)], the constructor;
    - [void clear(T *t)], the destructor;
    - [void copy(T *dst, T *src)], the copy assignment;
    - [intnat hash(T *t)] and [int compare(T *t1, T *t2)] for [Hashtbl.hash] and
      [compare];
    - [size_t mem(T *t)], the size of the memory owned by [t] outside of the [T]
      itself, so that the GC takes it into account (with
      [caml_alloc_custom_mem], like [sizeof(T)] which is always counted).

    [~dispose:"f"] adds to the OCaml file the function [f : ml -> unit], which
    clears and frees the value before it is collected (e.g. to close a file
    early). Using a disposed value raises [Invalid_argument].

    The [T] owned by OCaml is allocated with [malloc]. Its finalizer calls
    [clear] then [free]. [init_args] are C variables of other parameters, often
    created with {!extra_input}. *)

val custom_ptr :
  ?init:string ->
  ?clear:string ->
  ?hash:string ->
  ?compare:string ->
  ?mem:string ->
  ?dispose:string ->
  ?malloc:bool ->
  ml:string ->
  c:string ->
  unit ->
  Ty.mlc
(** The custom block contains a [c *], given to the C functions. By default it
    is the same as the [t] field of {!val:value_type}: allocated with [malloc]
    before [init], and freed after [clear].
    @param malloc
      [false] when the pointer is allocated by the C library (e.g. returned by
      its constructor) and freed by [clear], which receives the [c *]. *)

val custom :
  ?init:string ->
  ?clear:string ->
  ?hash:string ->
  ?compare:string ->
  ?mem:string ->
  ml:string ->
  c:string ->
  unit ->
  Ty.mlc
(** The C value [c] is stored directly in the custom block, it should be small
    (e.g. a pointer). The C functions take a [c *]. *)

val abstract : ?init:string -> ml:string -> c:string -> unit -> Ty.mlc
(** The C value [c] is stored directly in an OCaml block without finalizer. *)

(** {2 Parents and children}

    When a C value (the child) depends on another one (the parent), e.g. a
    statement on its database:
    - if the parent must only stay alive while the child is used, use
      {!keep_alive} on the type of the child;
    - if the parent is needed to destroy the child, use {!parent} and {!child},
      which count the references to the parent.

    In both cases the OCaml variable of the parent input is needed to create a
    child; {!Expert.param} returns it. *)

val keep_alive : Ty.mlc -> Expert.child
(** [keep_alive ty] is [ty] where an output also keeps alive the OCaml value of
    the parent. Use the field [t] for inputs and [with_parent p] for outputs. *)

val parent :
  ?clear:string -> ml:string -> c:string -> unit -> Ty.mlc * Expert.parent
(** [parent ~clear ~ml ~c ()] is a pointer [c] allocated by the C library.
    [void clear(c)] is called when the OCaml value and all its children are
    collected. *)

val child :
  Expert.parent ->
  ?clear:string ->
  ?clear_with_parent:string ->
  ml:string ->
  c:string ->
  unit ->
  Expert.child
(** [child parent ~ml ~c ()] is a pointer [c] allocated by the C library, which
    keeps a reference on its parent. When it is collected [void clear(c)] or
    [void clear_with_parent(parent_c, c)] is called, then the reference on the
    parent is released. Use the field [t] for inputs and [with_parent p] for
    outputs. *)

(** {1 Other types} *)

val algdata : string -> (string * (string * Ty.mlc) list) list -> Ty.mlc
(** [algdata name constrs] defines the OCaml algebraic datatype [name] and the
    corresponding C tagged union. *)

val enum : ?c:string -> string -> (string * string) list -> Ty.mlc
(** [enum name [ (constr, cst); ... ]] defines the OCaml type
    [type name = constr | ...] where each constructor is the C constant [cst] of
    type [c] (default [int]). The conversion to OCaml raises [Failure] for an
    unknown value. *)

val flags : ?c:string -> string -> (string * string) list -> Ty.mlc
(** Same as {!enum} for bit flags: [name list] in OCaml, the bitwise or of the
    constants in C. *)

val nullable : Ty.mlc -> Ty.mlc
(** [nullable ty] is [ty option] in OCaml, where [None] is the C [NULL] pointer,
    e.g. [nullable (string_nt ())]. *)

val string_as_FILE_ptr : Ty.mlc
(** A [string] output written by the C function in a [FILE *]. *)

val input_value : string -> Ty.param
(** [input_value ty] gives the OCaml value of type [ty] as a [value]. *)

val output_value : string -> Ty.param
(** [output_value ty] gives a [value *] to fill with an OCaml value of type
    [ty]. *)

val convert :
  ?c_to_mlc:string ->
  ?mlc_to_c:string ->
  ?using:Expr.Var.t list ->
  mlc:Ty.mlc ->
  c:Ty.c ->
  unit ->
  Ty.mlc
(** [convert ~mlc ~c] has the OCaml type of [mlc] and the C type [c]. The C
    conversion functions take a pointer to the destination, a pointer to the
    source, and the variables [using].
    @param c_to_mlc name of the conversion from [c] to the C type of [mlc]
    @param mlc_to_c name of the conversion from the C type of [mlc] to [c] *)

val on_stack :
  ?init_expr:string -> ?init:string -> ?clear:string -> string -> Ty.c
(** [on_stack ty] is the C type [ty] (for {!convert}) initialized and cleared
    with the given functions, which take a [ty *].
    @param init_expr the initial value (default [{ 0 }]) *)

val copy :
  Ty.mlc ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  Ty.mlc
(** [copy ty f] is [ty] where an output owned by C is copied with the C function
    [f] into a new value owned by OCaml. See [copy_of_ptr] in {!val:value_type}.
*)

val ret_option_if : Ty.mlc -> Ty.param * Ty.mlc
(** [ret_option_if ty] returns a parameter [status] (an [int *] for the C
    function) and the type [ty option], which is [None] if [status] is false. *)

(** {1 Generated code} *)

val module_ : string -> Expr.expr list -> Expr.expr
(** [module_ name l] puts the OCaml declarations [l] in the module [name]. *)

val ml_alias : string -> Ty.mlc -> Expr.expr
(** [ml_alias name ty] declares [type name] equal to the OCaml type of [ty]. *)

val get_expression : name:string -> Ty.mlc -> string -> Expr.expr
(** [get_expression ~name ty e] defines the OCaml function [name] that returns
    the value of the C expression [e]. *)

val do_nothing : string -> Ty.param list -> Expr.expr
(** [do_nothing name params] defines the OCaml function [name] that only
    converts its parameters, e.g. to create a value with its [init]. *)

val map_param_in_call :
  ?name:string ->
  ty:string ->
  Ty.param ->
  (('a -> Expr.expr -> 'b) -> Expr.expr -> 'c, 'a, 'b, 'd, 'd, 'c) format6 ->
  Ty.param
(** [map_param_in_call ~ty p "fmt"] gives to the C function the expression
    ["fmt"] of type [ty], applied to the C argument of [p]. *)

(** {1 Deprecated} *)

val int_trunc : Ty.mlc [@@deprecated "use c_int"]

val input_string :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
[@@deprecated "use string_len"]

val output_string :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
[@@deprecated "use string_len_ref"]

val input_array :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
[@@deprecated "use array_len"]

val output_array :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
[@@deprecated "use array_len_ref"]
