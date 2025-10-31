  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ output (ptr_ref int) "x"]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int camlid_int;
  typedef camlid_int * camlid_ref;
  static void camlid_init1(camlid_int * c){  }
  static void camlid_init(camlid_ref * c){ camlid_init1(*c); }
  void f(camlid_ref);
  static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_int(*c); }
  static void camlid_c2ml(value * v, camlid_ref * c){ camlid_c2ml1(v, *c); }
  static void camlid_free(camlid_ref * c){  }
  extern value camlid_stub_f(){
    camlid_ref x = &(((struct { camlid_int a; }) { 0 }).a);
    value ret;
    camlid_init(&x);
    f(x);
    camlid_c2ml(&ret, &x);
    camlid_free(&x);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c


  $ cat basic.ml
  type camlid_int = int
  type camlid_ptr_ref = camlid_int
  external f: unit -> camlid_ptr_ref = "camlid_stub_f"

  $ ocamlc -c basic.ml
