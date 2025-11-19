open Camlid
open Helper

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      (let a_len, a, len = output_array ~input:true int in
       func "f" ~ml:"f_output_input" [ a_len; a; len ]);
      (let a_len, a, len = output_array ~input:false int in
       func "f" ~ml:"f_output" [ a_len; a; len ]);
      (let a_len, a, len = input_array ~output:true int in
       func "f2" ~ml:"f_input_output" [ a_len; a; len ]);
      (let a_len, a, len = input_array ~output:false int in
       func "f2" ~ml:"f_input" [ a_len; a; len ]);
      (let a_len, len = fixed_length_array int in
       func "f4" [ len; a_len ]);
    ]
