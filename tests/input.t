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
  static void camlid_ml2c3(value * v3, camlid_int2 * c4){ *c4 = Int_val(*v3); }
  void f(camlid_int2);
  static void camlid_free5(camlid_int2 * c4){  }
  extern value camlid_stub_f1(value x0){
    camlid_int2 x1 = 0;
    value ret2;
    camlid_ml2c3(&x0, &x1);
    f(x1);
    ret2 = Val_unit;
    camlid_free5(&x1);
    return ret2;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int0 = int
  external f: camlid_int0 -> unit = "camlid_stub_f1"

  $ ocamlc -c basic.ml
