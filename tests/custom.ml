open Camlid
open Helper

let pointer =
  custom ~finalize:"finalize_ptr" ~compare:"compare_ptr" ~hash:"hash_ptr"
    ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file "test_custom" [ func ~declare:true "f" [ output pointer ] ]

let () = Utils.cat_and_compile "test_custom"
