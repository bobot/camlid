open Camlid
open Helper

let man = custom ~finalize_ptr:"Cudd_Quit" ~ml:"man" ~c:"DdManager*" ()
let mani = input man "man"
let bdd_t = typedef "bdd_t" "DdNode*"

let bdd_wrapper_s =
  Expert.declare_struct "bdd_wrapper"
    [ ("ptr", Expr.e_def bdd_t); ("manager", expr "DdManager*") ]

let bdd_wrapper = typedef "bdd_wrapper" "struct %a" pp_def bdd_wrapper_s

let bdd_finalize =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" pp_def bdd_wrapper) in
  {
    Expert.finalize =
      Type.code "bdd_finalize" "Cudd_RecursiveDeref(%a->manager,%a->ptr);"
        Expr.pp_var i Expr.pp_var i;
    i;
  }

let bdd_set =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" pp_def bdd_wrapper) in
  let c = Expr.Var.mk "c" (Expr.expr "%a*" pp_def bdd_t) in
  {
    Expert.set =
      Type.code "bdd_set" "Cudd_Ref(*%a);@ %a->manager=%a;@ %a->ptr=*%a;"
        Expr.pp_var c Expr.pp_var i Expr.pp_var mani.pc Expr.pp_var i
        Expr.pp_var c;
    i;
    c;
  }

let bdd_get =
  let i = Expr.Var.mk "i" (Expr.expr "%a*" pp_def bdd_wrapper) in
  let c = Expr.Var.mk "c" (Expr.expr "%a*" pp_def bdd_t) in
  {
    Expert.get = Type.code "bdd_get" "*%a=%a->ptr;" Expr.pp_var c Expr.pp_var i;
    i;
    c;
  }

let bdd =
  Expert.custom ~finalize:bdd_finalize ~set:bdd_set ~get:bdd_get ~ml:"bdd"
    ~icty:bdd_wrapper ~cty:bdd_t ()

let f_man fname mlname inputs result =
  func fname ~ml:mlname
    (mani :: List.map (fun ty -> input ty "v") inputs)
    ~result

let () =
  Generate.to_file ~in_header:true ~prefix:"caml_cudd_" ~headers:[ "./cudd.h" ]
    ~definitions:[ "./cudd_stub_def.h" ] "test_cudd"
    [
      (let cudd_init =
         Type.code ~ret:(expr "DdManager*") "cudd_init"
           "return Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);"
       in

       Expert.print_ml_fun
         {
           Expert.fid = cudd_init;
           mlname = "init";
           result =
             Some
               {
                 rty = man;
                 routput = true;
                 rc = Expr.Var.mk "res" (Expr.e_def man.cty);
                 binds = [];
               };
           params = [];
         });
      f_man "Cudd_ReadOne" "bdd_true" [] bdd;
      f_man "Cudd_ReadLogicZero" "bdd_false" [] bdd;
      f_man "Cudd_bddIthVar" "bdd_var" [ int_trunc ] bdd;
      f_man "Cudd_bddNewVar" "bdd_newvar" [] bdd;
      f_man "Cudd_bddAnd" "bdd_and" [ bdd; bdd ] bdd;
      f_man "Cudd_bddOr" "bdd_or" [ bdd; bdd ] bdd;
      func "Cudd_Not" ~ml:"bdd_not"
        [ { mani with used_in_call = false }; input bdd "v" ]
        ~result:bdd;
      (let b1 = input bdd "b" in
       let b2 = input bdd "b" in
       func_id
         (Type.code ~ret:(expr "int") "equal_bdd" "return (%a == %a);@,"
            Expr.pp_var b1.pc Expr.pp_var b2.pc)
         ~ml:"bdd_is_equal" [ b1; b2 ] ~result:bool);
      f_man "Cudd_bddLeq" "bdd_leq" [ bdd; bdd ] bool;
      (let b1 = input bdd "b" in
       func_id
         (Type.code "print"
            "fflush(stdout);@ Cudd_PrintMinterm(%a,%a);@ fflush(stdout);"
            Expr.pp_var mani.pc Expr.pp_var b1.pc)
         ~ml:"print" [ mani; b1 ]);
      (let ty =
         algdata "result"
           [
             ("False", []);
             ("True", []);
             ("Ifte", [ ("cond", int); ("then_", bdd); ("else_", bdd) ]);
           ]
       in
       func "bdd_inspect" ~ml:"inspect"
         [ mani; output (ptr_ref ty) "res"; input bdd "bdd" ]);
    ]

let () = Utils.cat_and_compile "test_cudd"
