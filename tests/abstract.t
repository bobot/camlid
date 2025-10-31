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
  typedef int * camlid_abstract2;
  static void camlid_init3(camlid_abstract2 * c2){  }
  void f(camlid_abstract2);
  typedef int * camlid_abstract_intern7;
  void id2(camlid_abstract_intern7 *, camlid_abstract2 *);
  static void camlid_c2ml5(value * v3, camlid_abstract2 * c2){
    *v3 = caml_alloc((sizeof(camlid_abstract_intern7) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
    id2(((camlid_abstract_intern7 *) Bp_val(*v3)), c2);
    }
  static void camlid_free6(camlid_abstract2 * c2){  }
  extern value camlid_stub_f1(){
    camlid_abstract2 x0 = ((camlid_abstract2) { });
    value ret1;
    camlid_init3(&x0);
    f(x0);
    camlid_c2ml5(&ret1, &x0);
    camlid_free6(&x0);
    return ret1;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

 
  $ cat basic.ml
  type camlid_myptr0
  external f: unit -> camlid_myptr0 = "camlid_stub_f1"

  $ ocamlc -c basic.ml
