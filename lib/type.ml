open Expr

(* untagging is considered as a kind of unboxing *)
type unbox_attribute = Unboxed | Untagged

type conv =
  | Boxed of { c2ml : code; ml2c : code }
  | Unboxable of {
      unbox_attribute : unbox_attribute;
      uty : expr;
      ml2u : code;
      u2ml : code;
      u2c : code;
      c2u : code;
      u : var;
      c2ml : code;
      ml2c : code;
    }

type typedef = {
  mlname : string option;  (** ml name *)
  cty : expr;  (** print the c type *)
  mlty : expr;  (** print the ocaml type *)
  conv : conv;  (** convert C values of this type to ML value *)
  init : code option;
      (** Initialize values of this type before giving them to stub function *)
  init_expr : expr;  (** expression initialization of the c version *)
  free : code option;
      (** Free the C memory allocated during the call (not accessible in output
          OCaml value) *)
  in_call : code option; (* default: variable c*)
  v : var; (* variable for the addresse of ml version *)
  c : var; (* variable for the addresse of c version *)
}

type pinput =
  | PINone
  | PIBoxed of { label : string option; ml : var; ml2c : code; pmlty : expr }
  | PIUnboxable of {
      label : string option;
      unbox_attribute : unbox_attribute;
      ml2u : code;
      u2c : code;
      u : var;
      ml : var;
      pmlty : expr;
    }

type poutput =
  | PONone
  | POBoxed of { ml : var; c2ml : code; pmlty : expr }
  | POUnboxable of {
      unbox_attribute : unbox_attribute;
      u2ml : code;
      c2u : code;
      u : var;
      ml : var;
      c2ml : code; (* used when more than one result*)
      pmlty : expr;
    }

type param = {
  pinput : pinput;
  poutput : poutput;
  pused_in_call : (var * expr) option;
  pinit : code option;
  pinit_expr : (var * expr option) list;
  pfree : code option;
}

type result = {
  routput : poutput; (* appears in ML results *)
  rfree : code option;
  rc : var;
}

type conf = Expr.expr list

let ty_binds ?(binds = []) ?v ?c ty =
  let bv = match v with None -> [] | Some v -> [ (ty.v, v) ] in
  let bc = match c with None -> [] | Some c -> [ (ty.c, c) ] in
  bv @ bc @ binds
