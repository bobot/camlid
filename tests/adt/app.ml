let () = Mylib.f_print Mylib.(T2)
let () = Mylib.f_print Mylib.(T1 (1, 2.0))
let () = Mylib.f_print Mylib.(T1 (3, 6.0))
let () = Mylib.f_print Mylib.(T3 2)
let () = Mylib.f_print Mylib.(T3 5)

let print_endline s =
  print_string s;
  print_newline ()

let () = Mylib.f_hello Mylib.Anonymous
let () = Mylib.f_hello Mylib.(Named "camlid")

let () =
  match Mylib.f_make 1 with
  | Mylib.Named s -> print_endline s
  | Mylib.Anonymous -> print_endline "anonymous"

let () =
  match Mylib.f_hello Mylib.(Named "a\000b") with
  | () -> print_endline "no error"
  | exception Failure msg -> print_endline msg
