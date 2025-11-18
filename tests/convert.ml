open Camlid
open Helper

module Result = struct
  let ty =
    algdata "result"
      [ ("Data", [ ("data", int) ]); ("Error", [ ("error", int) ]) ]
end

let data, data_or_status =
  let data = ignored (ptr_ref int) "data" in
  let to_ml : Expert.to_ml =
    let dst = Expr.Var.mk "dst" (expr "%a *" pp_def Result.ty.cty) in
    let src = Expr.Var.mk "src" (expr "%a *" pp_def int.cty) in
    {
      dst;
      src;
      to_ml =
        Type.code "to_ml" "combine_data_or_status(%a,%a,%a);" Expr.pp_var dst
          Expr.pp_var src Expr.pp_var data.pc;
    }
  in
  let ty = Expert.convert ~to_ml ~c:int ~ml:Result.ty () in
  (data, ty)

let () =
  Generate.to_file "test_convert" ~in_header:true
    ~definitions:[ "convert_defs.h" ]
    [
      func ~declare:true "f" ~result:data_or_status [ input int "other"; data ];
    ]

let () = Utils.cat_and_compile "test_convert"
