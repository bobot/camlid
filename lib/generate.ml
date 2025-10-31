open Expr

let print_c cout headers =
  output_string cout "#include <caml/mlvalues.h>\n";
  output_string cout "#include <caml/memory.h>\n";
  output_string cout "#include <caml/alloc.h>\n";
  output_string cout "#include <caml/custom.h>\n";
  List.iter
    (fun header -> Printf.fprintf cout "#include \"%s\"\n" header)
    headers

let to_file ?(prefix = "camlid") ?(headers = []) basename l =
  let cout_c = open_out (basename ^ "_stub.c") in
  print_c cout_c headers;
  let cout_ml = open_out (basename ^ ".ml") in
  let decl fmt f = f.expr fmt () in
  final_print ~prefix ~ml:cout_ml ~c:cout_c ML
    (expr "%a" Fmt.(list ~sep:Fmt.cut decl) l);
  close_out cout_c;
  close_out cout_ml
