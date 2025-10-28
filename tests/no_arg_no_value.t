  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f" []
  > ]
  > EOF

  $ cat -n basic_stub.c
       1	#include <caml/mlvalues.h>
       2	void f();
       3	extern value camlid_fun_f(){
       4	  f();
       5	  return Val_unit;
       6	  };

  $ ocamlc -c basic_stub.c

  $ cat -n basic.ml
       1	external f: unit -> unit = "camlid_fun_f"

  $ ocamlc -c basic.ml
