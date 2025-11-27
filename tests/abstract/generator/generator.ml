open Camlid
open Helper

let pointer = abstract ~initialize:"lib_init" ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "of_int" [ input int_trunc; output pointer ];
      func "to_int" [ input pointer ] ~result:int_trunc;
      func "lib_free" [ input pointer ];
    ]
