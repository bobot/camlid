  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [] ~result:int
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
  
  /* int: int */
  static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
  static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
  static void camlid_init_int(int * x){ };
  
  camlid_c_int f();
  extern value camlid_fun_f(){
    camlid_c_int c__res;
    value v__ret;
    c__res = f();
    camlid_c2ml_int(&v__ret,&c__res);
    return v__ret;
    };

  $ ocamlc -c basic_stub.c

  $ cat basic.ml
  (** int: int *)
  type camlid_ml_int = int
  
  external f: unit -> camlid_ml_int = "camlid_fun_f"

  $ ocamlc -c basic.ml
