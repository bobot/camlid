open Camlid
open Helper

let adt =
  let mlc =
    algdata "t"
      [
        ("T1", [ ("a", c_int); ("b", double) ]);
        ("T2", []);
        ("T3", [ ("c", c_int) ]);
      ]
  in
  let c = on_stack ~init_expr:"((lib_t) { { 0 } })" "lib_t" in
  convert ~c_to_mlc:"adt_of_lib_t" ~mlc_to_c:"lib_t_of_adt" ~c ~mlc ()

let name =
  let mlc =
    algdata "name" [ ("Anonymous", []); ("Named", [ ("s", string_nt ()) ]) ]
  in
  let c = on_stack ~clear:"name_clear" "name_t" in
  convert ~c_to_mlc:"name_of_name_t" ~mlc_to_c:"name_t_of_name" ~c ~mlc ()

let () =
  Generate.to_file ~in_header:true ~headers:[ "lib.h" ] "mylib"
    ~definitions:[ "defs.h" ]
    [
      func "f_print" [ input (ptr_ref adt) ];
      func "f_hello" [ input (ptr_ref name) ];
      func "f_make" [ output (ptr_ref name); input c_int ];
    ]
