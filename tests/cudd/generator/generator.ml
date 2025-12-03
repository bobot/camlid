open Camlid
open Helper

(** todo add reference counting on cudd manager *)

let man = custom_ptr ~finalize:"Cudd_Quit" ~ml:"man" ~c:"DdManager" ()
let mani, mani_pc = Expert.simple_param ~input:true man ~name:"man"
let bdd_t = Expert.typedef "bdd_t" "DdNode*"

let bdd_wrapper_s =
  Expert.declare_struct "bdd_wrapper"
    [ ("ptr", Expr.e_def bdd_t); ("manager", Expr.expr "DdManager*") ]

let bdd_wrapper =
  Expert.typedef "bdd_wrapper" "struct %a" Expr.pp_def bdd_wrapper_s

let bdd_finalize =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" Expr.pp_def bdd_wrapper) in
  {
    Expert.finalize =
      Expr.expr "Cudd_RecursiveDeref(%a->manager,%a->ptr);" Expr.pp_var i
        Expr.pp_var i;
    i;
  }

let bdd_set =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" Expr.pp_def bdd_wrapper) in
  let c = Expr.Var.mk "c" (Expr.expr "%a*" Expr.pp_def bdd_t) in
  {
    Expert.set =
      Expr.expr "Cudd_Ref(*%a);@ %a->manager=%a;@ %a->ptr=*%a;" Expr.pp_var c
        Expr.pp_var i Expr.pp_var mani_pc Expr.pp_var i Expr.pp_var c;
    i;
    c;
  }

let bdd_get =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" Expr.pp_def bdd_wrapper) in
  let c = Expr.Var.mk "c" (Expr.expr "%a*" Expr.pp_def bdd_t) in
  { Expert.get = Expr.expr "*%a=%a->ptr;" Expr.pp_var c Expr.pp_var i; i; c }

let bdd =
  Expert.custom ~finalize:bdd_finalize ~set:bdd_set ~get:bdd_get ~ml:"bdd"
    ~icty:(Expr.e_def bdd_wrapper) ~cty:(Expr.e_def bdd_t) ()

let f_man fname mlname inputs result =
  func fname ~ml:mlname (mani :: List.map (fun ty -> input ty) inputs) ~result

let () =
  Generate.to_file ~in_header:true ~prefix:"caml_cudd_" ~headers:[ "./cudd.h" ]
    ~definitions:[ "./defs.h" ] "mylib"
    [
      func ~ml:"init" "cudd_init" [] ~result:man;
      f_man "Cudd_ReadOne" "bdd_true" [] bdd;
      f_man "Cudd_ReadLogicZero" "bdd_false" [] bdd;
      f_man "Cudd_bddIthVar" "bdd_var" [ int_trunc ] bdd;
      f_man "Cudd_bddNewVar" "bdd_newvar" [] bdd;
      f_man "Cudd_bddAnd" "bdd_and" [ bdd; bdd ] bdd;
      f_man "Cudd_bddOr" "bdd_or" [ bdd; bdd ] bdd;
      func "Cudd_Not" ~ml:"bdd_not"
        [ { mani with pused_in_call = None }; input bdd ]
        ~result:bdd;
      func "equal_bdd" ~ml:"bdd_is_equal" [ input bdd; input bdd ] ~result:bool;
      f_man "Cudd_bddLeq" "bdd_leq" [ bdd; bdd ] bool;
      func "bdd_print" ~ml:"print" [ mani; input bdd ];
      (let ty =
         algdata "result"
           [
             ("False", []);
             ("True", []);
             ("Ifte", [ ("cond", int); ("then_", bdd); ("else_", bdd) ]);
           ]
       in
       func "bdd_inspect" ~ml:"inspect" [ mani; output (ptr_ref ty); input bdd ]);
    ]
