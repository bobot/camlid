open Camlid
open Helper

let () = Generate.to_file "test_no_arg_no_value" [ func ~declare:true "f" [] ]
let () = Utils.cat_and_compile "test_no_arg_no_value"
