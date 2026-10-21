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

type effects = Plan.effects = {
  alloc : bool;
      (** allocates in the OCaml heap, or registers values with [CAMLparam] *)
  raising : bool;  (** can raise an OCaml exception *)
}
(** The effects of a conversion on the OCaml runtime *)

let pure = Plan.pure
let allocates = { alloc = true; raising = false }
let may_raise = { alloc = false; raising = true }

(** The safe default when the effects are not known *)
let unknown = { alloc = true; raising = true }

let union e1 e2 =
  { alloc = e1.alloc || e2.alloc; raising = e1.raising || e2.raising }

type contract = {
  owned_by_ml : bool;
      (** [true] when the C value is owned by the OCaml value (custom blocks,
          bigarrays): [ml2c] borrows it, [c2ml] moves it into a new OCaml value
          and [cty.free] is [None]. [false] when [ml2c] and [c2ml] copy the
          data, and [cty.free] (if any) frees the C value. *)
  ml_heap : bool;
      (** [true] when the C value is or points into an OCaml value (e.g. a
          [value]), so the C function can't be called with the runtime lock
          released. *)
  ml2c : effects;  (** The effects of the boxed [ml2c] *)
  c2ml : effects;  (** The effects of the boxed [c2ml] *)
}
(** What the stub needs to know about a type to use it safely *)

let copied ?(ml_heap = false) ~ml2c ~c2ml () =
  { owned_by_ml = false; ml_heap; ml2c; c2ml }

let owned_by_ml ~ml2c ~c2ml () =
  { owned_by_ml = true; ml_heap = false; ml2c; c2ml }

type check_exn = Check_failure | Check_invalid_argument

type check = {
  cond : expr;
      (** True when the value is refused. It doesn't allocate, raise or modify
          anything. *)
  exn : check_exn;
  msg : string;
}
(** A check of a value before its conversion. A failed check raises the
    exception, when the stub owns nothing. *)

type checks = {
  ml2c_checks : check list;
      (** checks of the OCaml value [v]; they can read the C value of the inputs
          without destructor nor check (e.g. a length) *)
  c2ml_checks : check list;  (** checks of the C value [c] *)
}

let no_checks = { ml2c_checks = []; c2ml_checks = [] }

type mlc = {
  mlty : expr;  (** The OCaml type *)
  conv : conv;
  cty : c;
  v : var;  (** The OCaml value *)
  contract : contract;
  checks : checks;
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
  pml_heap : bool;  (** The C argument is or points into an OCaml value *)
  pml2c : effects;  (** The effects of the conversion of the input *)
  pc2ml : effects;  (** The effects of the boxed conversion of the output *)
  powned_by_ml : bool;  (** The C value is owned by the OCaml value *)
  pcheck_in : check list;  (** The checks of the input *)
  pcheck_out : check list;  (** The checks of the output *)
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
  rc2ml : effects;  (** The effects of the boxed conversion *)
  rowned_by_ml : bool;  (** The C value is owned by the OCaml value *)
  rcheck : check list;  (** The checks of the result *)
}

type conf = Expr.expr list
