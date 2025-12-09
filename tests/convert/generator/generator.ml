open Camlid
open Helper

let result =
  algdata "result"
    [ ("Data", [ ("data", int) ]); ("Error", [ ("error", int) ]) ]

let data, data_or_status =
  (* data_pc is a variable that will correspond to the C version of the parameter [data] in the stubbed function *)
  let data, data_pc =
    Expert.simple_param ~input:false ~output:false (ptr_ref int) ~name:"data"
  in
  (* A conversion function takes normally two arguments a pointer to the destination here of type [result] and the source here [int.cty]. For this case we are adding the variable [data_pc] as an additional argument. All the functions that use the C function `combine_data_or_status` have automatically this additional argument added. In the stub function the variable [data_pc] is applied to this additional argument *)
  let ty =
    convert ~c_to_mlc:"combine_data_or_status" ~c:int.cty ~mlc:result
      ~using:[ data_pc ] ()
  in
  (data, ty)

let () =
  Generate.to_file "mylib" ~in_header:true ~headers:[ "lib.h" ]
    ~definitions:[ "defs.h" ]
    [ func "f" ~result:data_or_status [ input int; data ] ]
