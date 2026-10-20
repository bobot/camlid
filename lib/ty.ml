(** Types and parameters.

    The fields are C code (of type {!type:Expr.expr}) that mention the variables
    [v] (OCaml value) and [c] (C value) of the record. *)

open Expr

(** Untagging is considered as a kind of unboxing *)
type unbox_attribute = Unboxed | Untagged

(** Conversions between the OCaml value [v] and the C value [c] *)
type conv =
  | Boxed of { c2ml : expr; ml2c : expr }
  | Unboxable of {
      unbox_attribute : unbox_attribute;
      uty : expr;
      ml2u : expr;
      u2ml : expr;
      u2c : expr;
      c2u : expr;
      u : var;
      c2ml : expr;
      ml2c : expr;
    }  (** In native code the value is given unboxed [u : uty] to the stub *)

type c = {
  cty : expr;  (** The C type *)
  init : expr option;
      (** Constructor: initializes [c] when it is not an input, before the call
      *)
  init_expr : expr;  (** Initial value of the variable [c] *)
  free : expr option;
      (** Destructor: called on [c] at the end of the stub, so it must not free
          memory moved into the OCaml results *)
  in_call : expr option;
      (** Expression given to the stubbed function (default [c]) *)
  c : var;  (** The C value *)
}
(** A C type and its lifecycle in the stub *)

type mlc = {
  mlty : expr;  (** The OCaml type *)
  conv : conv;
  cty : c;
  v : var;  (** The OCaml value *)
  owned_by_ml : bool;
      (** [true] when the C value is owned by the OCaml value (custom blocks,
          bigarrays): [ml2c] borrows it, [c2ml] moves it into a new OCaml value
          and [cty.free] is [None]. [false] when [ml2c] and [c2ml] copy the
          data, and [cty.free] (if any) frees the C value. *)
  ml_heap : bool;
      (** [true] when the C value is or points into an OCaml value (e.g. a
          [value]), so the C function can't be called with the runtime lock
          released. *)
  ml2c_noalloc : bool;
      (** [true] when the boxed [ml2c] doesn't allocate in the OCaml heap,
          doesn't raise, and doesn't use [CAMLparam] *)
  c2ml_noalloc : bool;  (** Same for the boxed [c2ml] *)
}
(** An OCaml type, a C type, and the conversions between them. It is a template
    instantiated by each parameter with fresh variables. *)

type pinput =
  | PINone
  | PIBoxed of { label : string option; ml : var; ml2c : expr; pmlty : expr }
  | PIUnboxable of {
      label : string option;
      unbox_attribute : unbox_attribute;
      ml2u : expr;
      u2c : expr;
      u : var;
      ml : var;
      pmlty : expr;
    }  (** C Expression used for the conversion of the input of a parameter *)

type poutput =
  | PONone
  | POBoxed of { ml : var; c2ml : expr; pmlty : expr }
  | POUnboxable of {
      unbox_attribute : unbox_attribute;
      u2ml : expr;
      c2u : expr;
      u : var;
      ml : var;
      c2ml : expr; (* used when more than one result*)
      pmlty : expr;
    }  (** C Expression used for the conversion of the output of a parameter *)

type param = {
  pinput : pinput;
  pinit_expr : (var * expr option) list;
  pinit : expr option;
  pused_in_call : (var * expr) option;
  pfree : expr option;
  poutput : poutput;
  pon_error : expr option;
      (** When the C function fails, moves an output owned by OCaml into an
          OCaml value, which will be finalized *)
  pml_heap : bool;  (** The C argument is or points into an OCaml value *)
  pnoalloc_in : bool;  (** The conversion of the input doesn't allocate *)
  pnoalloc_out : bool;
      (** The boxed conversion of the output doesn't allocate *)
}
(** A parameter is a set of variable linked by conversion expression. Possibly:
    - An input variable of type value and conversion functions
    - An unboxed input variable and conversion functions
    - C variables to declare locally
    - An expression used usually for initialisation
    - A parameter used in the call of the stubbed function
    - An unboxed output variable
    - An output variable of type value and conversion functions
    - An expression used usually for freeing memory *)

type result = {
  routput : poutput;
  rfree : expr option;
  rc : var;
  rnoalloc : bool;  (** The boxed conversion doesn't allocate *)
}

type conf = Expr.expr list
