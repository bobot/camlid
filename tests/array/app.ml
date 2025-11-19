let () = Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_output ())

let () =
  Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_output_input [| 1; 2; 3; 4 |])

let () = Mylib.f_input [| 1; 2; 3; 4 |]

let () =
  Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_input_output [| 1; 2; 3; 4 |])

let () = Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f4 4)
