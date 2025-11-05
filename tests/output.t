  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func ~declare:true "f" [ output (ptr_ref int) "x"];
  >  func ~declare:true "f2" [ output (ptr_ref int) "x"; output (ptr_ref int) "y"]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef intnat camlid_int;
  typedef camlid_int * camlid_ref;
  static void camlid_init1(camlid_int * c){  };
  static void camlid_init(camlid_ref * c){ camlid_init1(*c); };
  void f(camlid_ref);
  static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); };
  static void camlid_c2ml(value * v, camlid_ref * c){ camlid_c2ml1(v, *c); };
  static void camlid_free(camlid_ref * c){  };
  extern value camlid_stub_f(){
    camlid_ref x = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
    value ret;
    camlid_init(&x);
    f(x);
    camlid_c2ml(&ret, &x);
    camlid_free(&x);
    return ret;
  };
  typedef camlid_int * camlid_ref1;
  typedef camlid_int * camlid_ref2;
  static void camlid_init2(camlid_ref1 * c){ camlid_init1(*c); };
  static void camlid_init3(camlid_ref2 * c){ camlid_init1(*c); };
  void f2(camlid_ref1, camlid_ref2);
  static void camlid_c2ml2(value * v, camlid_ref1 * c){ camlid_c2ml1(v, *c); };
  static void camlid_c2ml3(value * v, camlid_ref2 * c){ camlid_c2ml1(v, *c); };
  static void camlid_free1(camlid_ref1 * c){  };
  static void camlid_free2(camlid_ref2 * c){  };
  extern value camlid_stub_f2(){
    camlid_ref1 x = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
    camlid_ref2 y = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
    value ret;
    value tup[2] = {Val_unit, Val_unit};
    camlid_init2(&x);
    camlid_init3(&y);
    f2(x, y);
    Begin_roots_block(tup, 2)
      camlid_c2ml2(&tup[0], &x);
      camlid_c2ml3(&tup[1], &y);
      ret = caml_alloc(2,0);
      Store_field(ret, 0, tup[0]);
      Store_field(ret, 1, tup[1]);
    End_roots()
    camlid_free1(&x);
    camlid_free2(&y);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c


  $ cat basic.ml
  type camlid_int = int
  type camlid_ptr_ref = camlid_int
  type camlid_ptr_ref1 = camlid_int
  type camlid_ptr_ref2 = camlid_int
  external f: unit -> camlid_ptr_ref = "camlid_stub_f"
  external f2: unit -> camlid_ptr_ref1 * camlid_ptr_ref2 = "camlid_stub_f2"

  $ ocamlc -c basic.ml
