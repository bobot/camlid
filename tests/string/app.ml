let () = Mylib.f_in "abcd"
let () = try Mylib.f_in "ab\000cd" with Failure msg -> print_endline msg

(* the first string is not copied, so it can't leak *)
let () = try Mylib.f_in2 "ab" "c\000d" with Failure msg -> print_endline msg
let () = Mylib.f_in_prefix "ab\000cd"
let () = print_string @@ Mylib.f_out ()
let () = print_string @@ Mylib.f_in3 3
let () = print_newline ()
let () = Mylib.f_print_n "abcdef" 3

let () =
  match Mylib.f_print_n "ab" 1000 with
  | () -> print_endline "no error"
  | exception Invalid_argument msg -> print_endline msg
