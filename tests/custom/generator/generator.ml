open Camlid
open Helper

let pointer =
  custom ~finalize:"finalize_ptr" ~compare:"compare_ptr" ~hash:"hash_ptr"
    ~initialize:"initialize_ptr" ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file "mylib" ~headers:[ "lib.h" ]
    [
      func "of_int" [ input int; output pointer ];
      func "to_int" [ input pointer ] ~result:int;
    ]
