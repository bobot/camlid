  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [] ~result:int
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int camlid_int2;
  camlid_int2 f();
  static void camlid_c2ml4(value * v2, camlid_int2 * c3){ *v2 = Val_int(*c3); }
  extern value camlid_stub_f1(){
    camlid_int2 res0;
    value ret1;
    res0 = f();
    camlid_c2ml4(&ret1, &res0);
    return ret1;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int0 = int
  external f: unit -> camlid_int0 = "camlid_stub_f1"

  $ ocamlc -c basic.ml
