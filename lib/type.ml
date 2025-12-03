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
  in_call : expr option; (* default: variable c*)
  c : var; (* variable for the addresse of c version *)
}

type typedef = {
  mlty : expr;  (** print the ocaml type *)
  conv : conv;  (** convert C values of this type to ML value *)
  cty : c;
  v : var; (* variable for the addresse of ml version *)
}

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
    }

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
    }

type param = {
  pinput : pinput;
  poutput : poutput;
  pused_in_call : (var * expr) option;
  pinit : expr option;
  pinit_expr : (var * expr option) list;
  pfree : expr option;
}

type result = {
  routput : poutput; (* appears in ML results *)
  rfree : expr option;
  rc : var;
}

type conf = Expr.expr list

let ty_binds ?(binds = []) ?v ?c ty =
  let bv = match v with None -> [] | Some v -> [ (ty.v, v) ] in
  let bc = match c with None -> [] | Some c -> [ (ty.cty.c, c) ] in
  bv @ bc @ binds
