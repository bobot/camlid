open Camlid
open Helper

let vec =
  value_type ~dispose:"vec_dispose" ~init:"vec_init" ~clear:"vec_clear" ~copy:"vec_set" ~ml:"vec"
    ~c:"vec" ()

let my_error = exception_ ~registered:"Mylib.My_error" "My_error"

let make =
  let n, n_var = Expert.simple_param ~input:true ~name:"n" int in
  func "make"
    ~error:
      (error ~exn:my_error ~msg:"error_msg" ~vars:[ n_var ] "is_negative_n")
    ~ignored_result:c_int
    [ output vec.t; output (ptr_ref (string_nt ())); n ]

(* the lock can't be released when the C function receives an OCaml value *)
let () =
  match func "f" ~release_lock:true [ input_value "int" ] with
  | _ -> Generate.to_file "should_not_exist" []
  | exception Invalid_argument _ -> ()

(* noalloc is impossible when the result is allocated *)
let () =
  match func "f" ~noalloc:true [ output (ptr_ref (string_nt ())) ] with
  | _ -> Generate.to_file "should_not_exist" []
  | exception Invalid_argument _ -> ()

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "checked_div" ~error:(error "is_negative") ~ignored_result:c_int
        [ input int; input int; output (ptr_ref int) ];
      make;
      func "slow_sum" ~release_lock:true [ input int ] ~result:int;
      func "vec_live" [] ~result:int;
      func "vec_length" [ input vec.t ] ~result:size_t;
      func ~ml:"vec_live_alloc" "vec_live" ~noalloc:false [] ~result:int;
    ]
