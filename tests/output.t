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
  /* int: int */
  typedef int camlid_c_int;
  static void camlid_c2ml_int(value *, camlid_c_int *);
  static void camlid_ml2c_int(camlid_c_int *, value *);
  static void camlid_init_int(camlid_c_int *);
  
  /* ref_int: ref on int */
  typedef camlid_c_int * camlid_c_ref_int;
  static void camlid_c2ml_ref_int(value *, camlid_c_ref_int *);
  static void camlid_ml2c_ref_int(camlid_c_ref_int *, value *);
  static void camlid_init_ref_int(camlid_c_ref_int *);
  
  /* int: int */
  static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
  static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
  static void camlid_init_int(int * x){ };
  
  /* ref_int: ref on int */
  static void camlid_c2ml_ref_int(value * v, camlid_c_int ** x){ camlid_c2ml_int(v,*x); };
  static void camlid_ml2c_ref_int(camlid_c_int ** x, value * v){ camlid_ml2c_int(*x,v); };
  static void camlid_init_ref_int(camlid_c_int ** x){ };
  
  void f(camlid_c_ref_int);
  extern value camlid_fun_f(){
    camlid_c_ref_int c_x = &(((struct { camlid_c_int a; }) { 0 }).a);
    value v__ret;
    camlid_init_ref_int(&c_x);
    f(c_x);
    camlid_c2ml_ref_int(&v__ret,&c_x);
    return v__ret;
    };

  $ ocamlc -c basic_stub.c


  $ cat basic.ml
  (** int: int *)
  type camlid_ml_int = int
  
  (** ref_int: ref on int *)
  type camlid_ml_ref_camlid_ml_int= camlid_ml_int
  
  external f: unit -> camlid_ml_ref_camlid_ml_int = "camlid_fun_f"

  $ ocamlc -c basic.ml
