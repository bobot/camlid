open Camlid
open Helper

let adt =
  let mlc =
    algdata "t"
      [
        ("T1", [ ("a", int_trunc); ("b", double) ]);
        ("T2", []);
        ("T3", [ ("c", int_trunc) ]);
      ]
  in
  let c = on_stack ~init_expr:"((lib_t) { { 0 } })" "lib_t" in
  convert ~c_to_mlc:"adt_of_lib_t" ~mlc_to_c:"lib_t_of_adt" ~c ~mlc ()

let () =
  Generate.to_file ~in_header:true ~headers:[ "lib.h" ] "mylib"
    ~definitions:[ "defs.h" ]
    [ func "f_print" [ input (ptr_ref adt) ] ]
