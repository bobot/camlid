open Camlid
open Helper

(* keep alive: the statement only needs its database while it is used *)
let db = custom_ptr ~malloc:false ~clear:"db_close" ~ml:"db" ~c:"db" ()

let stmt =
  keep_alive
    (custom_ptr ~malloc:false ~clear:"stmt_free" ~ml:"stmt" ~c:"stmt" ())

let stmt_new =
  let d, _, d_ml = Expert.param ~input:true ~name:"d" db in
  func "stmt_new" [ d ] ~result:(stmt.with_parent d_ml)

(* reference counting: the node needs its database to be freed *)
let rdb, rdb_parent = parent ~clear:"db_close" ~ml:"rdb" ~c:"db *" ()

let node =
  child rdb_parent ~clear_with_parent:"node_free" ~ml:"node" ~c:"node *" ()

let node_new =
  let d, _, d_ml = Expert.param ~input:true ~name:"d" rdb in
  func "node_new" [ d; input int ] ~result:(node.with_parent d_ml)

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "db_open" [ input int ] ~result:db;
      stmt_new;
      func "stmt_read" [ input stmt.t ] ~result:int;
      func ~ml:"rdb_open" "db_open" [ input int ] ~result:rdb;
      node_new;
      func "node_read" [ input node.t ] ~result:int;
      func "live" [] ~result:int;
    ]
