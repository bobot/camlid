let r = ref None
let () = r := Some (Sys.opaque_identity (Mylib.of_int 1))
let () = Printf.printf "%i\n%!" (Mylib.to_int (Option.get !r))
let () = r := None
let () = Gc.full_major ()
let r2 = ref None
let () = r2 := Some (Sys.opaque_identity (Mylib.two 2))

let () =
  let a, b = Option.get !r2 in
  Printf.printf "%i %i\n%!" (Mylib.to_int a) (Mylib.to_int b)

let () = r2 := None
let () = Gc.full_major ()
