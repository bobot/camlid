open Mylib

let pr = Printf.printf

let () =
  let v = vec_range 4 in
  vec_push v 10;
  pr "sum %d\n" (vec_sum v);
  let a = vec_ranges 4 in
  pr "sums %s\n"
    (String.concat " "
       (Array.to_list (Array.map (fun v -> string_of_int (vec_sum v)) a)));
  pr "total %d\n" (vec_total a);
  let g = vec_global () in
  vec_push g 5;
  pr "global copy %d, global %d\n" (vec_sum g) (vec_sum (vec_global ()));
  pr "equal %b %b\n" (vec_range 3 = vec_range 3) (vec_range 3 = vec_range 2);
  pr "hash %b\n" (Hashtbl.hash (vec_range 3) = Hashtbl.hash (vec_range 3));
  pr "zeros %d\n" (zeros_len (zeros ~size:3))

let () =
  Gc.full_major ();
  (* only the global vector remains *)
  pr "live %d\n" (vec_live ())
