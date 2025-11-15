open Camlid
open Helper

let pointer =
  abstract ~internal:"int *" ~get:"id1" ~set:"id2" ~ml:"myptr" ~c:"int *" ()

let () =
  Generate.to_file "test_abstract"
    [ func ~declare:true "f" [ output pointer "x" ] ]

let () = Utils.cat_and_compile "test_abstract"
