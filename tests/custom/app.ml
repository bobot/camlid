let r = ref None
let () = r := Some (Sys.opaque_identity (Mylib.of_int 1))
let () = Printf.printf "%i\n%!" (Mylib.to_int (Option.get !r))
let () = r := None
let () = Gc.full_major ()
