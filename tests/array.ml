open Camlid
open Helper

let () =
  Generate.to_file "test_array"
    [
      (let a_len, a, len = output_array ~input:true "a" int in
       func ~declare:true "f" [ a_len; a; len ]);
      (let a_len, a, len = output_array ~input:false "a" int in
       func ~declare:true "f1" [ a_len; a; len ]);
      (let a_len, a, len = input_array ~output:true "a" int in
       func ~declare:true "f2" [ a_len; a; len ]);
      (let a_len, a, len = input_array ~output:false "a" int in
       func ~declare:true "f3" [ a_len; a; len ]);
      (let a_len, len = output_set_length_array "a" int in
       func ~declare:true "f4" [ len; a_len ]);
    ]

let () = Utils.cat_and_compile "test_array"
