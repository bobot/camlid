(* generated using generator.exe and camlid *)
type man
type bdd
type caml_cudd_int = int
type caml_cudd_bool = bool
type caml_cudd_int1 = int
type result = | False | True | Ifte of caml_cudd_int1*bdd*bdd
type caml_cudd_ptr_ref = result
external init: unit -> man = "caml_cudd_stub_cudd_init"
external bdd_true: man -> bdd = "caml_cudd_stub_Cudd_ReadOne"
external bdd_false: man -> bdd = "caml_cudd_stub_Cudd_ReadLogicZero"
external bdd_var:
  man ->
  (caml_cudd_int [@untagged]) ->
  bdd
  = "caml_cudd_stub_Cudd_bddIthVar_byte" "caml_cudd_stub_Cudd_bddIthVar"
external bdd_newvar: man -> bdd = "caml_cudd_stub_Cudd_bddNewVar"
external bdd_and: man -> bdd -> bdd -> bdd = "caml_cudd_stub_Cudd_bddAnd"
external bdd_or: man -> bdd -> bdd -> bdd = "caml_cudd_stub_Cudd_bddOr"
external bdd_not: man -> bdd -> bdd = "caml_cudd_stub_Cudd_Not"
external bdd_is_equal:
  bdd ->
  bdd ->
  (caml_cudd_bool [@untagged])
  = "caml_cudd_stub_equal_bdd_byte" "caml_cudd_stub_equal_bdd"
external bdd_leq:
  man ->
  bdd ->
  bdd ->
  (caml_cudd_bool [@untagged])
  = "caml_cudd_stub_Cudd_bddLeq_byte" "caml_cudd_stub_Cudd_bddLeq"
external print: man -> bdd -> unit = "caml_cudd_stub_bdd_print"
external inspect:
  man ->
  bdd ->
  caml_cudd_ptr_ref
  = "caml_cudd_stub_bdd_inspect"