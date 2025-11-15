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
        Type.code "to_ml" "if(*%a){%a}{%a};" Expr.pp_var src
          (Expert.AlgData.make Result.data ~dst:(Expr.e_var dst)
             (Expr.e_var data.pc))
            .expr ()
          (Expert.AlgData.make Result.error ~dst:(Expr.e_var dst)
             (Expr.e_var src))
            .expr ();
    }
  in
  let ty = Expert.convert ~to_ml ~c:int ~ml:Result.ty () in
  ( data,
    {
      Type.rty = ty;
      routput = true;
      rc = Expr.Var.mk "res" (Expr.e_def ty.cty);
      binds = [];
    } )

let () =
  Generate.to_file "test_convert"
    [
      (let other_input = input int "other" in
       let fid =
         Expert.declare_existing ~result:(Expr.e_def int.cty) "f"
           [ other_input.pc; data.pc ]
       in
       Expert.print_ml_fun
         {
           mlname = "f";
           fid;
           params = [ data; other_input ];
           result = Some data_or_status;
         });
    ]

let () = Utils.cat_and_compile "test_convert"
