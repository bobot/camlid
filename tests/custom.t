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
  typedef int * camlid_custom2;
  static void camlid_init3(camlid_custom2 * c2){  }
  void f(camlid_custom2);
  void finalize_ptr(camlid_custom2 *);
  static void camlid_finalize_op7(value v4){
    finalize_ptr((camlid_custom2 *) Data_custom_val(v4));
    }
  int compare_ptr(camlid_custom2 *, camlid_custom2 *);
  static int camlid_compare_op8(value v15, value v26){
    return compare_ptr(
             (camlid_custom2 *) Data_custom_val(v15),
             (camlid_custom2 *) Data_custom_val(v26)
           );
    }
  intnat hash_ptr(camlid_custom2 *);
  static intnat camlid_hash_op9(value v7){
    return hash_ptr((camlid_custom2 *) Data_custom_val(v7));
    }
  struct custom_operations camlid_cops6 = {
  NULL,
  camlid_finalize_op7,
  camlid_compare_op8,
  camlid_hash_op9,
  custom_serialize_default,
  custom_deserialize_default
  };
  static void camlid_c2ml5(value * v3, camlid_custom2 * c2){
    *v3 = caml_alloc_custom(&camlid_cops6,sizeof(camlid_custom2), 0, 1);
    *((camlid_custom2 *) Data_custom_val(*v3)) = *c2;
    }
  extern value camlid_stub_f1(){
    camlid_custom2 x0 = ((camlid_custom2) { });
    value ret1;
    camlid_init3(&x0);
    f(x0);
    camlid_c2ml5(&ret1, &x0);
    return ret1;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c


  $ cat basic.ml
  type camlid_myptr0
  external f: unit -> camlid_myptr0 = "camlid_stub_f1"

  $ ocamlc -c basic.ml
