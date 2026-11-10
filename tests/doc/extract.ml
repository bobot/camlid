(* Prints the OCaml examples ({[ ... ]} blocks) of an .mld file as one OCaml
   file, in order, so that they are compiled and run by the tests. Blocks
   that are signatures (starting with "val") are skipped. *)

let () =
  let ic = open_in Sys.argv.(1) in
  let in_block = ref false and buf = Buffer.create 100 and line_no = ref 0 in
  let start = ref 0 in
  print_string "[@@@warning \"-32-26-27-33\"]\n\nopen Camlid\nopen Helper\n\n";
  (try
     while true do
       let l = input_line ic in
       incr line_no;
       if !in_block then
         if String.trim l = "]}" then begin
           in_block := false;
           let b = Buffer.contents buf in
           if not (String.length b >= 3 && String.sub b 0 3 = "val") then
             Printf.printf "# %i %S\n%s\n" !start Sys.argv.(1) b;
           Buffer.clear buf
         end
         else (
           Buffer.add_string buf l;
           Buffer.add_char buf '\n')
       else if String.trim l = "{[" then begin
         in_block := true;
         start := !line_no + 1
       end
     done
   with End_of_file -> ());
  close_in ic
