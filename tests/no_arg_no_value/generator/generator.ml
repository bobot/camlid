open Camlid
open Helper

let () = Generate.to_file ~headers:[ "lib.h" ] "mylib" [ func "f" [] ]
