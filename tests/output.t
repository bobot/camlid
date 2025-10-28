  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [ output (ptr_ref int) "x"]
  > ]
  > EOF

  $ cat -n basic_stub.c
       1	#include <caml/mlvalues.h>
       2	/* int: int */
       3	typedef int camlid_c_int;
       4	static void camlid_c2ml_int(value *, camlid_c_int *);
       5	static void camlid_ml2c_int(camlid_c_int *, value *);
       6	static void camlid_init_int(camlid_c_int *);
       7	
       8	/* ref_int: ref on int */
       9	typedef camlid_c_int * camlid_c_ref_int;
      10	static void camlid_c2ml_ref_int(value *, camlid_c_ref_int *);
      11	static void camlid_ml2c_ref_int(camlid_c_ref_int *, value *);
      12	static void camlid_init_ref_int(camlid_c_ref_int *);
      13	
      14	/* int: int */
      15	static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
      16	static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
      17	static void camlid_init_int(int * x){ };
      18	
      19	/* ref_int: ref on int */
      20	static void camlid_c2ml_ref_int(value * v, camlid_c_int ** x){ camlid_c2ml_int(v,*x); };
      21	static void camlid_ml2c_ref_int(camlid_c_int ** x, value * v){ camlid_ml2c_int(*x,v); };
      22	static void camlid_init_ref_int(int ** x){ };
      23	
      24	void f(camlid_c_ref_int);
      25	extern value camlid_fun_f(){
      26	  camlid_c_ref_int c_x = &(((struct { camlid_c_int a; }) { 0 }).a);
      27	  value v__ret;
      28	  camlid_init_ref_int(&c_x);
      29	  f(c_x);
      30	  camlid_c2ml_ref_int(&v__ret,&c_x);
      31	  return v__ret;
      32	  };

  $ ocamlc -c basic_stub.c


  $ cat -n basic.ml
       1	(** int: int *)
       2	type camlid_ml_int = int
       3	
       4	(** ref_int: ref on int *)
       5	type camlid_ml_ptr_camlid_ml_int= camlid_ml_int
       6	
       7	external f: unit -> camlid_ml_ptr_camlid_ml_int = "camlid_fun_f"

  $ ocamlc -c basic.ml
