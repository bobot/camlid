let () =
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
  let t = Thread.create (fun () -> Mylib.slow_sum 1000) () in
  Printf.printf "sum %i\n" (Mylib.slow_sum 1000);
  Thread.join t
