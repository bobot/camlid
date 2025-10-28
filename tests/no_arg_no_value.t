  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" []
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  void f();
  extern value camlid_fun_f(){
    f();
    return Val_unit;
    };

  $ ocamlc -c basic_stub.c

  $ cat basic.ml
  external f: unit -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
