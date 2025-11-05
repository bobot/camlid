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
  >  func ~declare:true "f" [ output pointer "x"]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int * camlid_custom;
  static void camlid_init(camlid_custom * c){  };
  void f(camlid_custom);
  void finalize_ptr(camlid_custom *);
  static void camlid_finalize_op(value v){
    finalize_ptr((camlid_custom *) Data_custom_val(v));
    };
  int compare_ptr(camlid_custom *, camlid_custom *);
  static int camlid_compare_op(value v1, value v2){
    return compare_ptr(
             (camlid_custom *) Data_custom_val(v1),
             (camlid_custom *) Data_custom_val(v2)
           );
    };
  intnat hash_ptr(camlid_custom *);
  static intnat camlid_hash_op(value v){
    return hash_ptr((camlid_custom *) Data_custom_val(v));
    };
  struct custom_operations camlid_cops = {
  NULL,
  camlid_finalize_op,
  camlid_compare_op,
  camlid_hash_op,
  custom_serialize_default,
  custom_deserialize_default
  };
  static void camlid_c2ml(value * v, camlid_custom * c){
    *v = caml_alloc_custom(&camlid_cops,sizeof(camlid_custom), 0, 1);
    *((camlid_custom *) Data_custom_val(*v)) = *c;
    };
  static void camlid_free(camlid_custom * c){  };
  extern value camlid_stub_f(){
    camlid_custom x = ((camlid_custom) { });
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
