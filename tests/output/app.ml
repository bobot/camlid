let print_f () = Printf.printf "%i\n" (Mylib.f ())
let () = print_f ()
let () = print_f ()

let print_f2 () =
  let i, j = Mylib.f2 () in
  Printf.printf "%i,%i\n" i j

let () = print_f2 ()
let () = print_f2 ()
