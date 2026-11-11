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

(* FLINT style: the context and the elements are allocated by camlid, and
   initialized with their parent *)
let ctx, ctx_parent =
  parent ~malloc:"ctx" ~init:"ctx_init" ~clear:"ctx_clear" ~ml:"ctx" ~c:"ctx *"
    ()

let elem =
  child ctx_parent ~malloc:"elem" ~init:"elem_init" ~parent_last:true
    ~clear_with_parent:"elem_clear" ~ml:"elem" ~c:"elem *" ()

let elem_funcs =
  let c, c_c, c_ml = Expert.param ~input:true ~name:"cx" ctx in
  let out = output (elem.with_parent ~c:c_c c_ml) in
  (* the arguments of elem_add must have the context [cx] as parent *)
  let same_parent =
    {
      Ty.ml2c_checks =
        [
          {
            cond =
              Expr.expr "%a != %a" Expr.pp_var c_c Expr.pp_expr
                (elem.parent_of elem.t.v);
            exn = Check_invalid_argument;
            msg = "elem_add: not the same context";
          };
        ];
      c2ml_checks = [];
    }
  in
  let a = input ~name:"a" { elem.t with checks = same_parent } in
  let b = input ~name:"b" { elem.t with checks = same_parent } in
  let func ?result ml name params =
    func ?result ~ml name (c :: params) ~call_params:(params @ [ c ])
  in
  [
    func "elem_of_int" "elem_set_si" [ out; input int ];
    func "elem_add" "elem_add" [ out; a; b ];
  ]
  @ [
      Helper.func "elem_get" [ input elem.t ] ~result:int;
      elem.get_parent "elem_ctx";
      Helper.func "ctx_elems" [ input ctx ] ~result:int;
    ]

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    ([
       func "db_open" [ input int ] ~result:db;
       stmt_new;
       func "stmt_read" [ input stmt.t ] ~result:int;
       func ~ml:"rdb_open" "db_open" [ input int ] ~result:rdb;
       node_new;
       func "node_read" [ input node.t ] ~result:int;
       do_nothing "ctx_create" [ output ctx ];
       func "live" [] ~result:int;
     ]
    @ elem_funcs)
