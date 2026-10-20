open Camlid
open Helper

let vec =
  value_type ~init:"vec_init" ~clear:"vec_clear" ~copy:"vec_set"
    ~hash:"vec_hash" ~compare:"vec_compare" ~ml:"vec" ~c:"vec" ()

(* The size is an argument of the OCaml function used only by [vec_init_n] *)
let size, size_var = extra_input ~label:"size" size_t

let zeros =
  value_type ~init:"vec_init_n" ~init_args:[ size_var ] ~clear:"vec_clear"
    ~copy:"vec_set" ~ml:"zeros" ~c:"vec" ()

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "vec_range" [ output vec.t; input size_t ];
      func "vec_push" [ input vec.t; input int ];
      func "vec_sum" [ input vec.t ] ~result:int;
      (let a = fixed_length_array ~len_used_in_call:true vec.value in
       func "vec_ranges" [ a.t; a.len ] ~ml:"vec_ranges");
      (let a = input_array vec.value in
       func "vec_total" [ a.t; a.len ] ~result:int);
      func "vec_global" [] ~result:vec.borrowed;
      func "vec_len" ~ml:"zeros" [ output zeros.t; size ] ~ignored_result:size_t;
      func "vec_len" ~ml:"zeros_len" [ input zeros.t ] ~result:size_t;
      func "vec_live" [] ~result:int;
    ]
