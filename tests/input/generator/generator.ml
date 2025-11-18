open Camlid
open Helper

let () =
  Generate.to_file "mylib"
    [ func "f" [ input int_trunc ] ]
    ~headers:[ "./lib.h" ]
