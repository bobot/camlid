open Camlid
open Helper

let () =
  Generate.to_file "mylib"
    [
      func "f" [ output (ptr_ref c_int) ];
      func "f2" [ output (ptr_ref c_int); output (ptr_ref c_int) ];
    ]
    ~headers:[ "lib.h" ]
