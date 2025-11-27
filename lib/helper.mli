(** {1 Function description} *)

val func :
  ?ml:string ->
  ?result:Type.typedef ->
  ?ignored_result:Type.typedef ->
  string ->
  Type.param list ->
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
      the generation of the C stub function. *)

(** {1 Creating parameters from templates} *)

(** The following functions create fresh variable, the same parameter should not
    be used multiple times in the description of the same C function. However it
    could be used in different descriptions *)

val input : ?name:string -> Type.typedef -> Type.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - appears in the arguments of the ML function
    - is used in the C call
    - is not present in the results.

    @param name Basename used for the fresh generated variable
    @param ty Template used for generatiing the parameter. *)

val output : ?name:string -> Type.typedef -> Type.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

val inout : ?name:string -> Type.typedef -> Type.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

val ignored : ?name:string -> Type.typedef -> Type.param
(** [input ~name ty] create an input parameter using the typedef template [ty].
    The parameter:
    - does not appear in the arguments of the ML function
    - is used in the C call
    - is not present in the results.

    @param name Basename used for the fresh generated variable
    @param ty template used for generatiing the parameter. *)

(** Common OCaml values *)

val int : Type.typedef
(** It is {!int} in OCaml, and [intptr_t] in C. The highest bit is lost when
    going from C to OCaml *)

val int_trunc : Type.typedef
(** It is {!int} in OCaml, and [int] in C. On 64bit, half of the highest bit are
    lost (63 bit long in OCaml and 32 bit long in C). *)

val double : Type.typedef
(** It is [float] in OCaml, and [double] in C. No loss during conversion. *)

val int32 : Type.typedef
(** It is {!int32} in OCaml, and [int32_t] in C. No loss during conversion. *)

val int64 : Type.typedef
(** It is {!int64} in OCaml, and [int64_t] in C. No loss during conversion. *)

val nativeint : Type.typedef
(** It is {!nativeint} in OCaml, and [intptr_t] in C. No loss during conversion.
*)

val bool : Type.typedef
(** It is {!bool} in OCaml, and [int] in C. No loss during conversion if the int
    is considered as a boolean. *)

val ptr_ref : Type.typedef -> Type.typedef
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

val string_nt : ?owned:bool -> unit -> Type.typedef
(** It is [string] in OCaml, and [char *] in C. In both side the conversion
    stops at the first null character. If there is no null character in the
    OCaml string it stops at the end. *)

val input_string :
  ?owned:bool -> ?output:bool -> ?name:string -> unit -> Type.param * Type.param

val output_string :
  ?owned:bool -> ?output:bool -> ?name:string -> unit -> Type.param * Type.param

val fixed_length_string :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  unit ->
  Type.param * Type.param

val input_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val output_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val fixed_length_array :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val abstract :
  ?initialize:string ->
  ?get:string ->
  ?set:string ->
  ?internal:string ->
  ml:string ->
  c:string ->
  unit ->
  Type.typedef

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
  Type.typedef

val custom_ptr :
  ?initialize:string ->
  ?finalize:string ->
  ?hash:string ->
  ?compare:string ->
  ?malloc:bool ->
  ml:string ->
  c:string ->
  unit ->
  Type.typedef

val algdata :
  string -> (string * (string * Type.typedef) list) list -> Type.typedef

val string_as_FILE_ptr : Type.typedef
val input_value : string -> Type.param
val output_value : string -> Type.param
val module_ : string -> Expr.expr list -> Expr.expr
