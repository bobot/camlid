open Camlid
open Helper

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "f_nat" [ inout (ptr_ref int) ];
      func "f_int" [ inout (ptr_ref int_trunc) ];
      func "f_double" [ inout (ptr_ref double) ];
      func "f_int32" [ inout (ptr_ref int32) ];
      func "f_int64" [ inout (ptr_ref int64) ];
      func "f_nat" ~ml:"f_nativeint" [ inout (ptr_ref nativeint) ];
    ]
