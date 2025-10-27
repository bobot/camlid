  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > 
  > let f = Type.{ fname = "f"; params = [{ pmode = In; pty = Helper.int; pname = "x" }];
  >           result = None; }
  > 
  > let () = Generate.to_file "basic" [Fun f]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  /* int: int */
  typedef int camlid_int;
  static void camlid_c2ml_int(value *, camlid_int *);
  static void camlid_ml2c_int(camlid_int *, value *);
  static void camlid_init_int(camlid_int *);
  
  /* int: int */
  static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
  static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
  static void camlid_init_int(int * x){ };
  
  void f(value);
  extern void camlid_fun_f(value v_x){
    camlid_int c_x;
    camlid_ml2c_int(&c_x,&v_x);
    f(c_x);
    };

  $ ocamlc -c basic_stub.c

  $ cat basic.ml
  (** int: int *)
  type camlid_int = int
  
  external f: camlid_int -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
