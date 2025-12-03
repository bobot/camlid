(* generated using generator.exe and camlid *)
type man
type bdd
type result = | False | True | Ifte of int*bdd*bdd
external init: unit -> man = "caml_cudd_stub_init"
external bdd_true: man -> bdd = "caml_cudd_stub_bdd_true"
external bdd_false: man -> bdd = "caml_cudd_stub_bdd_false"
external bdd_var:
  man ->
  (int [@untagged]) ->
  bdd
  = "caml_cudd_stub_bdd_var_byte" "caml_cudd_stub_bdd_var"
external bdd_newvar: man -> bdd = "caml_cudd_stub_bdd_newvar"
external bdd_and: man -> bdd -> bdd -> bdd = "caml_cudd_stub_bdd_and"
external bdd_or: man -> bdd -> bdd -> bdd = "caml_cudd_stub_bdd_or"
external bdd_not: man -> bdd -> bdd = "caml_cudd_stub_bdd_not"
external bdd_is_equal:
  bdd ->
  bdd ->
  (bool [@untagged])
  = "caml_cudd_stub_bdd_is_equal_byte" "caml_cudd_stub_bdd_is_equal"
external bdd_leq:
  man ->
  bdd ->
  bdd ->
  (bool [@untagged])
  = "caml_cudd_stub_bdd_leq_byte" "caml_cudd_stub_bdd_leq"
external print: man -> bdd -> unit = "caml_cudd_stub_print"
external inspect: man -> bdd -> result = "caml_cudd_stub_inspect"