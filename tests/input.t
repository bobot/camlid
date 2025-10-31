  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ input int_trunc "x"]
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
  typedef int camlid_int;
  static void camlid_ml2c(value * v, camlid_int * c){ *c = Int_val(*v); }
  void f(camlid_int);
  static void camlid_free(camlid_int * c){  }
  extern value camlid_stub_f(value x){
    camlid_int x1 = ((camlid_int) { });
    value ret;
    camlid_ml2c(&x, &x1);
    f(x1);
    ret = Val_unit;
    camlid_free(&x1);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int = int
  external f: camlid_int -> unit = "camlid_stub_f"

  $ ocamlc -c basic.ml
