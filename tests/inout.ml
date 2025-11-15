open Camlid
open Helper

let () =
  Generate.to_file "test_inout"
    [
      func ~declare:true "f_nat" [ inout int "x" ];
      func ~declare:true "f_int" [ inout int_trunc "x" ];
      func ~declare:true "f_double" [ inout double "x" ];
      func ~declare:true "f_int32" [ inout int32 "x" ];
      func ~declare:true "f_int64" [ inout int64 "x" ];
      func ~declare:true "f_nativeint" [ inout nativeint "x" ];
    ]

let () = Utils.cat_and_compile "test_inout"
