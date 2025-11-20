open Camlid
open Helper

module Result = struct
  let ty =
    algdata "result"
      [ ("Data", [ ("data", int) ]); ("Error", [ ("error", int) ]) ]
end

let data, data_or_status =
  let data, data_pc = Expert.simple_param' (ptr_ref int) ~name:"data" in
  let conv =
    Expert.mk_converter ~dst:Result.ty ~src:int "combine_data_or_status"
      (fun ~src ~dst -> [ dst; src; data_pc ])
  in
  let ty = Expert.convert ~b_to_a:conv ~b:int ~a:Result.ty () in
  (data, ty)

let () =
  Generate.to_file "mylib" ~in_header:true ~definitions:[ "defs.h" ]
    [ func ~declare:true "f" ~result:data_or_status [ input int; data ] ]
