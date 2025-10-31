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
  typedef int camlid_int4;
  typedef camlid_int4 * camlid_ref3;
  static void camlid_init8(camlid_int4 * c3){  }
  static void camlid_init5(camlid_ref3 * c2){ camlid_init8(*c2); }
  void f(camlid_ref3);
  static void camlid_c2ml9(value * v5, camlid_int4 * c3){ *v5 = Val_int(*c3); }
  static void camlid_c2ml7(value * v4, camlid_ref3 * c2){
    camlid_c2ml9(v4, *c2);
    }
  extern value camlid_stub_f1(){
    camlid_ref3 x0 = &(((struct { camlid_int4 a; }) { 0 }).a);
    value ret1;
    camlid_init5(&x0);
    f(x0);
    camlid_c2ml7(&ret1, &x0);
    return ret1;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c


  $ cat basic.ml
  type camlid_int2 = int
  type camlid_ptr_ref0 = camlid_int2
  external f: unit -> camlid_ptr_ref0 = "camlid_stub_f1"

  $ ocamlc -c basic.ml
