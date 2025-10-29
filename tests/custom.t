  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let pointer = custom
  > ~finalize:"finalize_ptr"
  > ~compare:"compare_ptr"
  > ~hash:"hash_ptr"
  > ~ml:"myptr" ~c:"int *" ()
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
  /* custom_myptr: abstract tag for type "int *" */
  typedef int * camlid_c_custom_myptr;
  static void camlid_c2ml_custom_myptr(value *, camlid_c_custom_myptr *);
  static void camlid_ml2c_custom_myptr(camlid_c_custom_myptr *, value *);
  static void camlid_init_custom_myptr(camlid_c_custom_myptr *);
  
  /* custom_myptr: abstract tag for type "int *" */
  void finalize_ptr(camlid_c_custom_myptr *);
  static void camlid_ops_finalize_myptr(value v)
  {
  finalize_ptr((camlid_c_custom_myptr *) Data_custom_val(v));
  }
  int compare_ptr(camlid_c_custom_myptr *, camlid_c_custom_myptr *);
  static int camlid_ops_compare_myptr(value v1, value v2)
  {
  return compare_ptr((camlid_c_custom_myptr *) Data_custom_val(v1), (camlid_c_custom_myptr *) 
  Data_custom_val(v2));
  }
  long hash_ptr(camlid_c_custom_myptr *);
  static long camlid_ops_hash_myptr(value v)
  {
  return hash_ptr((camlid_c_custom_myptr *) Data_custom_val(v));
  }
  struct custom_operations camlid_cops_myptr = {
  NULL,
  camlid_ops_finalize_myptr,
  camlid_ops_compare_myptr,
  camlid_ops_hash_myptr,
  custom_serialize_default,
  custom_deserialize_default
  };
  static void camlid_c2ml_custom_myptr(value * v, camlid_c_custom_myptr * c){
    *v = caml_alloc_custom(&camlid_cops_myptr,sizeof(camlid_c_custom_myptr), 0, 1);
    *((camlid_c_custom_myptr *) Data_custom_val(*v)) = *c;
  };
  static void camlid_ml2c_custom_myptr(camlid_c_custom_myptr * c, value * v){
    *c = *((camlid_c_custom_myptr *)  Data_custom_val(*v));
  };
  static void camlid_init_custom_myptr(camlid_c_custom_myptr * c){  };
  
  void f(camlid_c_custom_myptr);
  extern value camlid_fun_f(){
    camlid_c_custom_myptr c_x = ((camlid_c_custom_myptr) { });
    value v__ret;
    camlid_init_custom_myptr(&c_x);
    f(c_x);
    camlid_c2ml_custom_myptr(&v__ret,&c_x);
    return v__ret;
    };

  $ ocamlc -c basic_stub.c


  $ cat basic.ml
  (** custom_myptr: abstract tag for type "int *" *)
  type myptr
  
  external f: unit -> myptr = "camlid_fun_f"

  $ ocamlc -c basic.ml
