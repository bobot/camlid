  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ input int "x"]
  > ]
  > ~headers:["./basic.h"]
  > EOF

  $ cat > basic.h <<EOF
  > void f(int);
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  #include "./basic.h"
  typedef int camlid_int2;
  static void camlid_ml2c3(value * v2, camlid_int2 * c3){ *c3 = Int_val(*v2); }
  void f(camlid_int2);
  extern value camlid_stub_f1(value x0){
    camlid_int2 x1 = 0;
    camlid_ml2c3(&x0, &x1);
    f(x1);
    return Val_unit;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int0 = int
  external f: camlid_int0 -> unit = "camlid_stub_f1"

  $ ocamlc -c basic.ml
