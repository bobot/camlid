(* generated using generator.exe and camlid *)
type db
type stmt
type rdb
type node
external db_open:
  (int [@untagged]) ->
  db
  = "camlid_stub_db_open_byte" "camlid_stub_db_open"
external stmt_new: db -> stmt = "camlid_stub_stmt_new"
external stmt_read:
  stmt ->
  (int [@untagged])
  = "camlid_stub_stmt_read_byte" "camlid_stub_stmt_read" [@@noalloc]
external rdb_open:
  (int [@untagged]) ->
  rdb
  = "camlid_stub_rdb_open_byte" "camlid_stub_rdb_open"
external node_new:
  rdb ->
  (int [@untagged]) ->
  node
  = "camlid_stub_node_new_byte" "camlid_stub_node_new"
external node_read:
  node ->
  (int [@untagged])
  = "camlid_stub_node_read_byte" "camlid_stub_node_read" [@@noalloc]
external live:
  unit -> (int [@untagged])
  = "camlid_stub_live_byte" "camlid_stub_live" [@@noalloc]
