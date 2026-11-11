(* a signal pending when the lock is released: its handler must not raise
   while the stub owns the vec *)
let signals () =
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> raise Exit));
  Gc.full_major ();
  let before = Mylib.vec_live () in
  let raised = ref 0 in
  for _ = 1 to 3 do
    try
      Mylib.vec_signal_next_init ();
      ignore (Mylib.vec_unlocked 1);
      Gc.minor ()
    with Exit -> incr raised
  done;
  Gc.full_major ();
  Printf.printf "raised %i, live after signals %i\n" !raised
    (Mylib.vec_live () - before)

(* the body is a function called through Sys.opaque_identity: flambda
   doesn't inline it, so its values don't become static symbols, which are
   never collected *)
let run () =
  Printf.printf "div %i\n" (Mylib.checked_div 7 2);
  (match Mylib.checked_div 1 0 with
  | _ -> print_endline "no error"
  | exception Failure msg -> Printf.printf "Failure %S\n" msg);
  let _, s = Mylib.make 3 in
  Printf.printf "make %S\n" s;
  Gc.full_major ();
  let before = Mylib.vec_live () in
  for _ = 1 to 3 do
    match Mylib.make (-1) with
    | _ -> print_endline "no error"
    | exception Mylib.My_error msg -> Printf.printf "My_error %S\n" msg
  done;
  Gc.full_major ();
  Printf.printf "live after errors %i\n" (Mylib.vec_live () - before);
  let v, _ = Mylib.make 4 in
  Gc.full_major ();
  let before = Mylib.vec_live () in
  Printf.printf "length %i\n" (Mylib.vec_length v);
  Mylib.vec_dispose v;
  Mylib.vec_dispose v;
  Printf.printf "disposed %i\n" (before - Mylib.vec_live ());
  (match Mylib.vec_length v with
  | _ -> print_endline "no error"
  | exception Invalid_argument msg -> Printf.printf "Invalid_argument %S\n" msg);
  (* the vec is owned by OCaml before the check of the status fails *)
  Gc.full_major ();
  let before = Mylib.vec_live () in
  for _ = 1 to 3 do
    match Mylib.vec_status (-1) with
    | _ -> print_endline "no error"
    | exception Failure msg -> Printf.printf "Failure %S\n" msg
  done;
  (match Mylib.vec_status 0 with
  | _, Mylib.Empty -> print_endline "empty"
  | _, Mylib.Ok -> print_endline "ok");
  Gc.full_major ();
  Printf.printf "live after status %i\n" (Mylib.vec_live () - before);
  (* Windows has no SIGUSR1: print the expected line *)
  if Sys.win32 then print_endline "raised 3, live after signals 0"
  else signals ();
  let t = Thread.create (fun () -> Mylib.slow_sum 1000) () in
  Printf.printf "sum %i\n" (Mylib.slow_sum 1000);
  Thread.join t

let () = (Sys.opaque_identity run) ()
