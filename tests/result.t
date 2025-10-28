  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" [] ~result:int
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
       8	/* int: int */
       9	static void camlid_c2ml_int(value * v, int * x){ *v = Val_int(*x); };
      10	static void camlid_ml2c_int(int * x, value * v){ *x = Int_val(*v); };
      11	static void camlid_init_int(int * x){ };
      12	
      13	camlid_c_int f();
      14	extern value camlid_fun_f(){
      15	  camlid_c_int c__res;
      16	  value v__ret;
      17	  c__res = f();
      18	  camlid_c2ml_int(&v__ret,&c__res);
      19	  return v__ret;
      20	  };

  $ ocamlc -c basic_stub.c

  $ cat basic.ml
  (** int: int *)
  type camlid_ml_int = int
  
  external f: unit -> camlid_ml_int = "camlid_fun_f"

  $ ocamlc -c basic.ml
