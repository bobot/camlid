(** {1 Function description} *)

val func :
  ?ml:string ->
  ?result:Ty.mlc ->
  ?ignored_result:Ty.mlc ->
  ?call_params:Ty.param list ->
  string ->
  Ty.param list ->
  Expr.expr
(** Describe the signature of a C function and generate the stub. Three
    variants:
    - [func ~ml c_name params] : the C function has a [void] result (no result)
    - [func ~ml ~result c_name params]: the C function has a result with
      template [result], and it is part of the OCaml results
    - [func ~ml ~ignored_result c_name params]: C function has a result with
      template [result], and it is ignored for the OCaml function

    The resulting expression is the OCaml declaration for the external. Since it
    mentions the {!type:Camlid.Expr.id} of the C stub function, it is
    automatically added to the C file during the generation by
    {!Camlid.Generate.to_file}.

    The order of the inputs, outputs (result first), and arguments of the C
    function all use the order given by [params].

    @param ml name to use for the OCaml function
    @param c_name is the name of the C function
    @param params
      usually parameters of the C function. More generally when using
      {!Camlid.Expert}, it gives all the variables and their functions used for
      the generation of the C stub function.
    @param call_params
      allows to reorder the list of parameters for the call to the stubbed
      function (by default params) *)

(** {1 Creating parameters from templates} *)

(** The following functions create fresh variable, the same parameter should not
    be used multiple times in the description of the same C function. However it
    could be used in different descriptions *)

val input : ?name:string -> Ty.mlc -> Ty.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - appears in the arguments of the ML function
    - is used in the C call
    - is not present in the results.

    @param name Basename used for the fresh generated variable
    @param ty Template used for generatiing the parameter. *)

val output : ?name:string -> Ty.mlc -> Ty.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

val inout : ?name:string -> Ty.mlc -> Ty.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

val ignored : ?name:string -> Ty.mlc -> Ty.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is not present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

(** Common OCaml values *)

val int : Ty.mlc
(** It is {!int} in OCaml, and [intptr_t] in C. The highest bit is lost when
    going from C to OCaml *)

val size_t : Ty.mlc
(** It is {!int} in OCaml, and [size_t] in C. The highest bit is lost when going
    from C to OCaml. Unsigned version of {!int} *)

val int_trunc : Ty.mlc
(** It is {!int} in OCaml, and [int] in C. On 64bit, half of the highest bit are
    lost (63 bit long in OCaml and 32 bit long in C). *)

val double : Ty.mlc
(** It is [float] in OCaml, and [double] in C. No loss during conversion. *)

val int32 : Ty.mlc
(** It is {!int32} in OCaml, and [int32_t] in C. No loss during conversion. *)

val int64 : Ty.mlc
(** It is {!int64} in OCaml, and [int64_t] in C. No loss during conversion. *)

val nativeint : Ty.mlc
(** It is {!nativeint} in OCaml, and [intptr_t] in C. No loss during conversion.
*)

val bool : Ty.mlc
(** It is {!bool} in OCaml, and [int] in C. No loss during conversion if the int
    is considered as a boolean. *)

val ptr_ref : Ty.mlc -> Ty.mlc
(** [ptr_def ty] it change the C value used when calling the C function to a
    pointer on the given type. It is useful with {!output}. The pointer is valid
    for the duration og the call. The OCaml type is the same as for [ty]. *)

(** Allocated C structure *)

(** In all the following section, [owned] indicates if the OCaml sides owns the
    C allocated memory, and so it it should free it want it is not using it
    anymore. An OCaml value (e.g. a [string]) is always owned by the OCaml side,
    the question is if the memory allocated by the stub and given to the C
    function, or the memory returned by the C function is owned by the OCaml
    side and should be freed.

    Except in the case of bigarrays, or user defined abstract and custom type
    the allocated memory is freed at the end of the stub *)

(** In all this section, [init] indicates if the stub need to allocate the
    memory before calling the C function *)

val string_nt : ?owned:bool -> unit -> Ty.mlc
(** It is [string] in OCaml, and [char *] in C. The conversion to C raises
    [Failure] if the OCaml string contains a null character. *)

val string_nt_prefix : ?owned:bool -> unit -> Ty.mlc
(** Same as {!string_nt} but the conversion to C silently stops at the first
    null character. *)

type with_length = {
  t : Ty.param; (* array parameter *)
  len : Ty.param; (* length of the array *)
}

val input_string :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
(** Parameter for a "char*" and its length. Given as such to the stubbed
    function. Converted from/to a [string] in ocaml. *)

val output_string :
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?name:string ->
  unit ->
  with_length
(** Parameter for a "char*" and its length. The address of those variables are
    given to the stubbed function. Converted from/to a [string] in ocaml. *)

val fixed_length_string :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  unit ->
  with_length
(** Parameter for a "char*" converted (if input) or allocated (if output) with
    the length given as input. [(fixed_length_string ()).len] is an input
    parameter. Converted from/to a [string] in ocaml. *)

val input_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [input_array mlc] Parameter for an array of [mlc] and its length. Given as
    such to the stubbed function. Converted from/to an array of [mlc] in OCaml.
*)

val output_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [output_array mlc] Parameter for an array of [mlc] and its length. The
    address of those variables are given to the stubbed function. Converted
    from/to an array of [mlc] in OCaml. *)

val fixed_length_array :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  Ty.mlc ->
  with_length
(** [fixed_length_array mlc] Parameter for an array of [mlc] converted (if
    input) or allocated (if output) with the length given as input.
    [(fixed_length_array ()).len] is an input parameter. Converted from/to an
    array of [mlc] in OCaml.*)

val abstract :
  ?initialize:string ->
  ?get:string ->
  ?set:string ->
  ?internal:string ->
  ml:string ->
  c:string ->
  unit ->
  Ty.mlc

val custom :
  ?initialize:string ->
  ?finalize:string ->
  ?hash:string ->
  ?compare:string ->
  ?get:string ->
  ?set:string ->
  ?internal:string ->
  ml:string ->
  c:string ->
  unit ->
  Ty.mlc

val custom_ptr :
  ?initialize:string ->
  ?finalize:string ->
  ?hash:string ->
  ?compare:string ->
  ?malloc:bool ->
  ml:string ->
  c:string ->
  unit ->
  Ty.mlc

val algdata : string -> (string * (string * Ty.mlc) list) list -> Ty.mlc
val string_as_FILE_ptr : Ty.mlc
val input_value : string -> Ty.param
val output_value : string -> Ty.param
val module_ : string -> Expr.expr list -> Expr.expr
val ml_alias : string -> Ty.mlc -> Expr.expr

val copy :
  Ty.mlc ->
  ?vars:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.Var.t list) ->
  ?exprs:(dst:Expr.Var.t -> src:Expr.Var.t -> Expr.expr list) ->
  string ->
  Ty.mlc

val ret_option_if : Ty.mlc -> Ty.param * Ty.mlc
val get_expression : name:string -> Ty.mlc -> string -> Expr.expr

val map_param_in_call :
  ?name:string ->
  ty:string ->
  Ty.param ->
  (('a -> Expr.expr -> 'b) -> Expr.expr -> 'c, 'a, 'b, 'd, 'd, 'c) format6 ->
  Ty.param

val do_nothing : string -> Ty.param list -> Expr.expr

val convert :
  ?c_to_mlc:string ->
  ?mlc_to_c:string ->
  ?using:Expr.Var.t list ->
  mlc:Ty.mlc ->
  c:Ty.c ->
  unit ->
  Ty.mlc

val on_stack :
  ?init_expr:string -> ?initialize:string -> ?clear:string -> string -> Ty.c
(** Define a C type that is allocated on the stack. It is initialized and
    cleared with the given functions.
    @param init_expr is the initialization expression used *)
