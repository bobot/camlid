let x = Mylib.of_int 1
let () = Printf.printf "%i\n%!" (Mylib.to_int x)
let () = Mylib.lib_free x
