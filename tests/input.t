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

  $ cat -n basic_stub.c
       1	#include <caml/mlvalues.h>
       2	#include "./basic.h"
       3	/* int: int */
       4	typedef int camlid_c_int;
       5	static void camlid_c2ml_int(value *, camlid_c_int *);
       6	static void camlid_ml2c_int(camlid_c_int *, value *);
       7	static void camlid_init_int(camlid_c_int *);
       8	
       9	/* int: int */
      10	static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
      11	static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
      12	static void camlid_init_int(int * x){ };
      13	
      14	void f(camlid_c_int);
      15	extern value camlid_fun_f(value v_x){
      16	  camlid_c_int c_x = 0;
      17	  camlid_ml2c_int(&c_x,&v_x);
      18	  f(c_x);
      19	  return Val_unit;
      20	  };

  $ ocamlc -c basic_stub.c

  $ cat -n basic.ml
       1	(** int: int *)
       2	type camlid_ml_int = int
       3	
       4	external f: camlid_ml_int -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
