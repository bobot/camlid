open Camlid
open Helper

let pointer = abstract ~init:"lib_init" ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "of_int" [ input c_int; output pointer ];
      func "to_int" [ input pointer ] ~result:c_int;
      func "lib_free" [ input pointer ];
    ]
