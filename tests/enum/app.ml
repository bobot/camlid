let pp_color = function
  | Mylib.Red -> "Red"
  | Mylib.Green -> "Green"
  | Mylib.Blue -> "Blue"

let pp_perm l =
  String.concat ","
    (List.map
       (function Mylib.Read -> "Read" | Write -> "Write" | Exec -> "Exec")
       l)

let pp_opt = function None -> "None" | Some s -> Printf.sprintf "Some %S" s

let () =
  List.iter
    (fun c ->
      Printf.printf "%s -> %s\n" (pp_color c) (pp_color (Mylib.next_color c)))
    [ Red; Green; Blue ];
  (match Mylib.bad_color () with
  | c -> print_endline (pp_color c)
  | exception Failure msg -> Printf.printf "Failure %S\n" msg);
  Printf.printf "[%s]\n" (pp_perm (Mylib.flags_add_exec []));
  Printf.printf "[%s]\n" (pp_perm (Mylib.flags_add_exec [ Write; Read ]));
  List.iter
    (fun s ->
      Printf.printf "%s -> %s\n" (pp_opt s) (pp_opt (Mylib.non_empty s)))
    [ None; Some ""; Some "abc" ]
