open Camlid
open Helper

let string = string_nt ()

let () =
  Generate.to_file "mylib"
    [
      func "f_in" [ input string ];
      func "f_in2" [ input string; input string ];
      func ~ml:"f_in_prefix" "f_in" [ input (string_nt_prefix ()) ];
      func "f_out" [ output (ptr_ref string) ];
      (let str = fixed_length_string () in
       func "f_in3" [ str.t; str.len ]);
      (let str =
         fixed_length_string ~input:true ~output:false ~len_used_in_call:true ()
       in
       func "f_print_n" [ str.t; str.len ]);
      (let str = string_len () in
       func "f_len" [ str.t; str.len ]);
      (let str = string_len_ref () in
       func "f_out_len" [ str.t; str.len ]);
    ]
    ~headers:[ "./lib.h" ]
