  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func ~declare:true "f" [] ~result:int
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef intnat camlid_int;
  camlid_int f();
  static void camlid_c2ml(value * v, camlid_int * c){ *v = Val_long(*c); };
  extern value camlid_stub_f(){
    camlid_int res;
    value ret;
    res = f();
    camlid_c2ml(&ret, &res);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int = int
  external f: unit -> camlid_int = "camlid_stub_f"

  $ ocamlc -c basic.ml
