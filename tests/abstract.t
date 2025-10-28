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
  /* abstract_myptr: abstract tag for type "int *" */
  typedef int * camlid_c_abstract_myptr;
  static void camlid_c2ml_abstract_myptr(value *, camlid_c_abstract_myptr *);
  static void camlid_ml2c_abstract_myptr(camlid_c_abstract_myptr *, value *);
  static void camlid_init_abstract_myptr(camlid_c_abstract_myptr *);
  
  /* abstract_myptr: abstract tag for type "int *" */
  typedef int * camlid_c_custom_intern_myptr;
  void id1(camlid_c_abstract_myptr *, camlid_c_custom_intern_myptr *);
  void id2(camlid_c_custom_intern_myptr *, camlid_c_abstract_myptr *);
  static void camlid_c2ml_abstract_myptr(value * v,camlid_c_abstract_myptr * x){
  *v = caml_alloc((sizeof(camlid_c_custom_intern_myptr) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  id2(((camlid_c_custom_intern_myptr *) Bp_val(*v)),x); };
  static void camlid_ml2c_abstract_myptr(camlid_c_abstract_myptr * x,value * v){ id1(x,((camlid_c_custom_intern_myptr *) Bp_val(*v))); };
  static void camlid_init_abstract_myptr(camlid_c_abstract_myptr * x){ };
  
  void f(camlid_c_abstract_myptr);
  extern value camlid_fun_f(){
    camlid_c_abstract_myptr c_x = ((camlid_c_abstract_myptr) { });
    value v__ret;
    camlid_init_abstract_myptr(&c_x);
    f(c_x);
    camlid_c2ml_abstract_myptr(&v__ret,&c_x);
    return v__ret;
    };

  $ ocamlc -c basic_stub.c

 
  $ cat basic.ml
  (** abstract_myptr: abstract tag for type "int *" *)
  type myptr
  
  external f: unit -> myptr = "camlid_fun_f"

  $ ocamlc -c basic.ml
