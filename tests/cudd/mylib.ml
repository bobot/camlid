(* generated using generator.exe and camlid *)
type caml_cudd_man
type caml_cudd_bdd
type caml_cudd_int = int
type caml_cudd_bool = bool
type caml_cudd_int1 = int
type result =
  | False
  | True
  | Ifte of caml_cudd_int1*caml_cudd_bdd*caml_cudd_bdd
type caml_cudd_ptr_ref = result
external init: unit -> caml_cudd_man = "caml_cudd_stub_cudd_init"
external bdd_true:
  caml_cudd_man ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_ReadOne"
external bdd_false:
  caml_cudd_man ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_ReadLogicZero"
external bdd_var:
  caml_cudd_man ->
  (caml_cudd_int [@untagged]) ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_bddIthVar_byte" "caml_cudd_stub_Cudd_bddIthVar"
external bdd_newvar:
  caml_cudd_man ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_bddNewVar"
external bdd_and:
  caml_cudd_man ->
  caml_cudd_bdd ->
  caml_cudd_bdd ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_bddAnd"
external bdd_or:
  caml_cudd_man ->
  caml_cudd_bdd ->
  caml_cudd_bdd ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_bddOr"
external bdd_not:
  caml_cudd_man ->
  caml_cudd_bdd ->
  caml_cudd_bdd
  = "caml_cudd_stub_Cudd_Not"
external bdd_is_equal:
  caml_cudd_bdd ->
  caml_cudd_bdd ->
  (caml_cudd_bool [@untagged])
  = "caml_cudd_stub_equal_bdd_byte" "caml_cudd_stub_equal_bdd"
external bdd_leq:
  caml_cudd_man ->
  caml_cudd_bdd ->
  caml_cudd_bdd ->
  (caml_cudd_bool [@untagged])
  = "caml_cudd_stub_Cudd_bddLeq_byte" "caml_cudd_stub_Cudd_bddLeq"
external print:
  caml_cudd_man ->
  caml_cudd_bdd ->
  unit
  = "caml_cudd_stub_bdd_print"
external inspect:
  caml_cudd_man ->
  caml_cudd_bdd ->
  caml_cudd_ptr_ref
  = "caml_cudd_stub_bdd_inspect"