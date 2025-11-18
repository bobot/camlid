open Camlid
open Helper

module Result = struct
  let ty =
    algdata "result"
      [ ("Data", [ ("data", int) ]); ("Error", [ ("error", int) ]) ]
end

let data, data_or_status =
  let data = ignored (ptr_ref int) "data" in
  let conv =
    Expert.mk_converter ~dst:Result.ty ~src:int "combine_data_or_status"
      (fun ~src ~dst -> [ dst; src; data.pc ])
  in
  let ty = Expert.convert ~b_to_a:conv ~b:int ~a:Result.ty () in
  (data, ty)

let () =
  Generate.to_file "test_convert" ~in_header:true
    ~definitions:[ "convert_defs.h" ]
    [
      func ~declare:true "f" ~result:data_or_status [ input int "other"; data ];
    ]

let () = Utils.cat_and_compile "test_convert"
