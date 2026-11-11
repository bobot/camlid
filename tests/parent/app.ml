let full_major () =
  Gc.full_major ();
  Gc.full_major ()

(* the body is a function called through Sys.opaque_identity: flambda
   doesn't inline it, so its values don't become static symbols, which are
   never collected *)
let run () =
  (* the database is only reachable from the statements *)
  let stmts = List.init 3 (fun _ -> Mylib.stmt_new (Mylib.db_open 10)) in
  full_major ();
  List.iter (fun s -> Printf.printf "stmt %i\n" (Mylib.stmt_read s)) stmts;
  (* the database is only reachable from the nodes *)
  let nodes =
    let d = Mylib.rdb_open 100 in
    List.init 3 (fun i -> Mylib.node_new d i)
  in
  full_major ();
  List.iter (fun n -> Printf.printf "node %i\n" (Mylib.node_read n)) nodes;
  (* the context is only reachable from the elements *)
  let elems =
    let c = Mylib.ctx_create () in
    let one = Mylib.elem_of_int c 1 in
    List.init 3 (fun i -> Mylib.elem_add c one (Mylib.elem_of_int c i))
  in
  full_major ();
  List.iter (fun e -> Printf.printf "elem %i\n" (Mylib.elem_get e)) elems

let () =
  (Sys.opaque_identity run) ();
  full_major ();
  Printf.printf "live %i\n" (Mylib.live ())
