let () = Mylib.f_in "abcd"
let () = try Mylib.f_in "ab\000cd" with Failure msg -> print_endline msg
let () = Mylib.f_in_prefix "ab\000cd"
let () = print_string @@ Mylib.f_out ()
let () = print_string @@ Mylib.f_in3 3
