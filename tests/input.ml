open Camlid
open Helper

let () =
  let cout = open_out "test_input.h" in
  output_string cout "void f(int);";
  close_out cout

let () =
  Generate.to_file "test_input"
    [ func "f" [ input int_trunc "x" ] ]
    ~headers:[ "./test_input.h" ]

let () = Utils.cat_and_compile "test_input"
