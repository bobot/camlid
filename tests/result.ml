open Camlid
open Helper

let () =
  Generate.to_file "test_result" [ func ~declare:true "f" [] ~result:int ]

let () = Utils.cat_and_compile "test_result"
