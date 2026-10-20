open Expr

(* untagging is considered as a kind of unboxing *)
type unbox_attribute = Unboxed | Untagged

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
    }

type c = {
  cty : expr;  (** print the c type *)
  init : expr option;
      (** Initialize values of this type before giving them to stub function *)
  init_expr : expr;  (** expression initialization of the c version *)
  free : expr option;
      (** Free the C memory allocated during the call (not accessible in output
          OCaml value) *)
  in_call : expr option;
      (* expression used in the call of the stubbed function (default: variable c) *)
  c : var;
      (* variable of the c version used in the expression of the previous fields  *)
}
(** A C type with utility functions *)

type mlc = {
  mlty : expr;  (** print the ocaml type *)
  conv : conv;  (** convert C values of this type to ML value *)
  cty : c;
  v : var;
      (* variable for the ml version used in the expression of the previous fields *)
}
(** The conversion between an ML and C type *)

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

type result = { routput : poutput; rfree : expr option; rc : var }
type conf = Expr.expr list
