open Camlid
open Helper

let result =
  algdata "result"
    [ ("Data", [ ("data", int) ]); ("Error", [ ("error", int) ]) ]

let data, data_or_status =
  let data, data_pc =
    Expert.simple_param ~input:false ~output:false (ptr_ref int) ~name:"data"
  in
  let ty =
    convert ~c_to_mlc:"combine_data_or_status" ~c:int.cty ~mlc:result
      ~using:[ data_pc ] ()
  in
  (data, ty)

let () =
  Generate.to_file "mylib" ~in_header:true ~headers:[ "lib.h" ]
    ~definitions:[ "defs.h" ]
    [ func "f" ~result:data_or_status [ input int; data ] ]
