let () = Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_output ())

let () =
  Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_output_input [| 1; 2; 3; 4 |])

let () = Mylib.f_input [| 1; 2; 3; 4 |]

let () =
  Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f_input_output [| 1; 2; 3; 4 |])

let () = Fmt.pr "%a@." Fmt.(vbox (array int)) (Mylib.f4 4)
let () = Fmt.pr "%d@." (Mylib.f_strings [| "a"; "bc"; "def" |])
let () = Fmt.pr "%d@." (Mylib.f_sum [| 1; 2; 3 |] 3)
let () = Fmt.pr "%d@." (Mylib.f_sum [| 1; 2; 3 |] 2)

let () =
  match Mylib.f_sum [| 1; 2 |] 1000 with
  | n -> Fmt.pr "no error %d@." n
  | exception Invalid_argument msg -> Fmt.pr "Invalid_argument %s@." msg
