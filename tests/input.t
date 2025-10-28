  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > 
  > let f = Type.{ fname = "f";
  > params =
  >   [{ input = true;
  >      output = false;
  >      pty = Helper.int;
  >      pname = "x" }];
  > result = None; }
  > 
  > let () = Generate.to_file "basic" [Fun f]
  > EOF

  $ cat -n basic_stub.c
       1	#include <caml/mlvalues.h>
       2	/* int: int */
       3	typedef int camlid_c_int;
       4	static void camlid_c2ml_int(value *, camlid_c_int *);
       5	static void camlid_ml2c_int(camlid_c_int *, value *);
       6	static void camlid_init_int(camlid_c_int *);
       7	
       8	/* int: int */
       9	static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
      10	static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
      11	static void camlid_init_int(int * x){ };
      12	
      13	void f(camlid_c_int);
      14	extern value camlid_fun_f(value v_x){
      15	  camlid_c_int c_x = 0;
      16	  camlid_ml2c_int(&c_x,&v_x);
      17	  f(c_x);
      18	  return Val_unit;
      19	  };

  $ ocamlc -c basic_stub.c

  $ cat -n basic.ml
       1	(** int: int *)
       2	type camlid_ml_int = int
       3	
       4	external f: camlid_ml_int -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
