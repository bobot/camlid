open Camlid
open Helper

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      (let a = array_len_ref ~input:true int in
       func "f" ~ml:"f_output_input" [ a.t; a.len ]);
      (let a = array_len_ref ~input:false int in
       func "f" ~ml:"f_output" [ a.t; a.len ]);
      (let a = array_len ~output:true int in
       func "f2" ~ml:"f_input_output" [ a.t; a.len ]);
      (let a = array_len ~output:false int in
       func "f2" ~ml:"f_input" [ a.t; a.len ]);
      (let a = array_len (string_nt ()) in
       func "f_strings" [ a.t; a.len ] ~result:size_t);
      (let a = fixed_length_array int in
       func "f4" [ a.len; a.t ]);
    ]
