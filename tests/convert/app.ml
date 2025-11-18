let pp cout = function
  | Mylib.Error i -> Printf.fprintf cout "Error: %i" i
  | Mylib.Data i -> Printf.fprintf cout "Data: %i" i

let () =
  for i = 0 to 3 do
    Printf.printf "%a\n%!" pp (Mylib.f i)
  done
