  $ ../camlid_toplevel.exe -stdin < gen.ml
  /tmp/dune_cram_ee7943_.cram.sh/main.sh: 1: /tmp/dune_cram_ee7943_.cram.sh/1.sh: cannot open gen.ml: No such file
  [2]

  $ cat cudd_core_stub.c
  cat: cudd_core_stub.c: No such file or directory
  [1]

  $ ocamlc -ccopt --warn-all -c cudd_core_stub.c
  cc1: fatal error: cudd_core_stub.c: No such file or directory
  compilation terminated.
  [2]

  $ cat cudd_core.ml
  cat: cudd_core.ml: No such file or directory
  [1]

  $ ocamlc -c cudd_core.ml
  File "cudd_core.ml", line 1:
  Error: I/O error: cudd_core.ml: No such file or directory
  [2]
