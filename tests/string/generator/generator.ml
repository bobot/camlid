open Camlid
open Helper

let string = string_nt ()

let () =
  Generate.to_file "mylib"
    [
      func "f_in" [ input string ];
      func "f_out" [ output (ptr_ref string) ];
      (let str = fixed_length_string () in
       func "f_in3" [ str.t; str.len ]);
    ]
    ~headers:[ "./lib.h" ]
