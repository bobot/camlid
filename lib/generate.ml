open Expr

let print_c cout =
  output_string cout "#include <caml/mlvalues.h>\n";
  output_string cout "#include <caml/memory.h>\n";
  output_string cout "#include <caml/alloc.h>\n";
  output_string cout "#include <caml/custom.h>\n"

let to_file ?(in_header = false) ?(prefix = "camlid_") ?(headers = []) basename
    l =
  let cout_c = open_out (basename ^ "_stub.c") in
  let filename_h = basename ^ "_stub.h" in
  let cout_h = if in_header then open_out filename_h else cout_c in
  print_c cout_c;
  if in_header then (
    output_string cout_h
      (Printf.sprintf "#ifndef %s\n#define %s\n" basename basename);
    output_string cout_c (Printf.sprintf "#include \"%s\"\n" filename_h));
  List.iter
    (fun header -> Printf.fprintf cout_h "#include \"%s\"\n" header)
    headers;
  let cout_ml = open_out (basename ^ ".ml") in
  let decl fmt f = f.expr fmt () in
  final_print ~prefix ~ml:cout_ml ~c:cout_c ~h:cout_h ML
    (expr "%a" Fmt.(list ~sep:Fmt.cut decl) l);
  if in_header then (
    output_string cout_h (Printf.sprintf "#endif");
    close_out cout_h);
  close_out cout_c;
  close_out cout_ml
