open Camlid
open Helper

let () =
  Generate.to_file "mylib"
    [
      func "f" [ output (ptr_ref int_trunc) ];
      func "f2" [ output (ptr_ref int_trunc); output (ptr_ref int_trunc) ];
    ]
    ~headers:[ "lib.h" ]
