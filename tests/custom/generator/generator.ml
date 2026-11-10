open Camlid
open Helper

let pointer =
  custom ~clear:"finalize_ptr" ~compare:"compare_ptr" ~hash:"hash_ptr"
    ~init:"initialize_ptr" ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file "mylib" ~headers:[ "lib.h" ]
    [
      func "of_int" [ input c_int; output pointer ];
      func "to_int" [ input pointer ] ~result:c_int;
      func "two" [ input c_int; output pointer; output pointer ];
    ]
