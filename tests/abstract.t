  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let pointer = abstract ~internal:"int *" ~get:"id1" ~set:"id2" ~ml:"myptr" ~c:"int *" ()
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ output pointer "x"]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int * camlid_abstract;
  static void camlid_init(camlid_abstract * c){  }
  void f(camlid_abstract);
  typedef int * camlid_abstract_intern;
  void id2(camlid_abstract_intern *, camlid_abstract *);
  static void camlid_c2ml(value * v, camlid_abstract * c){
    *v = caml_alloc((sizeof(camlid_abstract_intern) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
    id2(((camlid_abstract_intern *) Bp_val(*v)), c);
    }
  static void camlid_free(camlid_abstract * c){  }
  extern value camlid_stub_f(){
    camlid_abstract x = ((camlid_abstract) { });
    value ret;
    camlid_init(&x);
    f(x);
    camlid_c2ml(&ret, &x);
    camlid_free(&x);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

 
  $ cat basic.ml
  type camlid_myptr
  external f: unit -> camlid_myptr = "camlid_stub_f"

  $ ocamlc -c basic.ml
