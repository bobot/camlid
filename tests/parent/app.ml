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
  List.iter (fun e -> Printf.printf "elem %i\n" (Mylib.elem_get e)) elems;
  (* the context is given back by an element, and is only reachable from the
     returned value *)
  let c =
    let c0 = Mylib.ctx_create () in
    Mylib.elem_ctx (Mylib.elem_of_int c0 7)
  in
  full_major ();
  let e = Mylib.elem_of_int c 4 in
  Printf.printf "elem %i, has elems %b\n" (Mylib.elem_get e)
    (Mylib.ctx_elems c > 0);
  (* two values of the same context are equal *)
  let c' = Mylib.elem_ctx e in
  Printf.printf "same ctx: %b %b %b, other ctx: %b\n" (c == c') (c = c')
    (Hashtbl.hash c = Hashtbl.hash c')
    (c = Mylib.ctx_create ());
  (* an element of another context is refused *)
  try ignore (Mylib.elem_add (Mylib.ctx_create ()) e e)
  with Invalid_argument msg -> Printf.printf "Invalid_argument %s\n" msg

let () =
  (Sys.opaque_identity run) ();
  full_major ();
  Printf.printf "live %i\n" (Mylib.live ())
