open Camlid
open Helper

let () =
  Generate.to_file "test_output"
    [
      func ~declare:true "f" [ output (ptr_ref int) ];
      func ~declare:true "f2" [ output (ptr_ref int); output (ptr_ref int) ];
    ]

let () = Utils.cat_and_compile "test_output"
