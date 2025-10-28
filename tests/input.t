  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ input int "x"]
  > ]
  > ~headers:["./basic.h"]
  > EOF

  $ cat > basic.h <<EOF
  > void f(int);
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  #include "./basic.h"
  /* int: int */
  typedef int camlid_c_int;
  static void camlid_c2ml_int(value *, camlid_c_int *);
  static void camlid_ml2c_int(camlid_c_int *, value *);
  static void camlid_init_int(camlid_c_int *);
  
  /* int: int */
  static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
  static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
  static void camlid_init_int(int * x){ };
  
  void f(camlid_c_int);
  extern value camlid_fun_f(value v_x){
    camlid_c_int c_x = 0;
    camlid_ml2c_int(&c_x,&v_x);
    f(c_x);
    return Val_unit;
    };

  $ ocamlc -c basic_stub.c

  $ cat basic.ml
  (** int: int *)
  type camlid_ml_int = int
  
  external f: camlid_ml_int -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
