let () = Printf.printf "%i" (Mylib.f_nat 1)
let () = Printf.printf "%i" (Mylib.f_int 1)
let () = Printf.printf "%f" (Mylib.f_double 1.0)
let () = Printf.printf "%li" (Mylib.f_int32 1l)
let () = Printf.printf "%Li" (Mylib.f_int64 1L)

let () =
  Printf.printf "%s"
    (Nativeint.to_string (Mylib.f_nativeint (Nativeint.of_int 1)))
