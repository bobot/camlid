open Camlid
open Helper

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      (let a = output_array ~input:true int in
       func "f" ~ml:"f_output_input" [ a.t; a.len ]);
      (let a = output_array ~input:false int in
       func "f" ~ml:"f_output" [ a.t; a.len ]);
      (let a = input_array ~output:true int in
       func "f2" ~ml:"f_input_output" [ a.t; a.len ]);
      (let a = input_array ~output:false int in
       func "f2" ~ml:"f_input" [ a.t; a.len ]);
      (let a = fixed_length_array int in
       func "f4" [ a.len; a.t ]);
    ]
