open Camlid
open Helper

let () =
  Generate.to_file "test_inout"
    [
      func ~declare:true "f_nat" [ inout int ];
      func ~declare:true "f_int" [ inout int_trunc ];
      func ~declare:true "f_double" [ inout double ];
      func ~declare:true "f_int32" [ inout int32 ];
      func ~declare:true "f_int64" [ inout int64 ];
      func ~declare:true "f_nativeint" [ inout nativeint ];
    ]

let () = Utils.cat_and_compile "test_inout"
