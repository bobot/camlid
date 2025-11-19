open Expr

let print_h cout =
  output_string cout "#include <stddef.h>\n";
  output_string cout "#include <inttypes.h>\n"

let print_c cout =
  output_string cout "#include <caml/mlvalues.h>\n";
  output_string cout "#include <caml/memory.h>\n";
  output_string cout "#include <caml/alloc.h>\n";
  output_string cout "#include <caml/custom.h>\n";
  output_string cout "#include <string.h>\n"

let header copen cclose cout =
  Printf.fprintf cout "%sgenerated using %s and camlid%s\n%!" copen
    (Filename.basename Sys.executable_name)
    cclose

let header_c = header "// " ""
let header_ml = header "(* " " *)"

let to_file ?(in_header = false) ?(prefix = "camlid_") ?(headers = [])
    ?(definitions = []) basename l =
  let cout_c = open_out (basename ^ "_stub.c") in
  header_c cout_c;
  let filename_h = basename ^ "_stub.h" in
  let cout_h = if in_header then open_out filename_h else cout_c in
  if in_header then (
    header_c cout_h;
    output_string cout_h
      (Printf.sprintf "#ifndef %s\n#define %s\n" basename basename);
    output_string cout_c (Printf.sprintf "#include \"%s\"\n" filename_h));
  print_h cout_h;
  print_c cout_c;
  List.iter
    (fun header -> Printf.fprintf cout_h "#include \"%s\"\n" header)
    headers;
  List.iter
    (fun header -> Printf.fprintf cout_c "#include \"%s\"\n" header)
    definitions;
  let cout_ml = open_out (basename ^ ".ml") in
  header_ml cout_ml;
  let decl fmt f = f.expr fmt () in
  final_print ~prefix ~ml:cout_ml ~c:cout_c ~h:cout_h ML
    (expr "%a" Fmt.(list ~sep:Fmt.cut decl) l);
  if in_header then (
    output_string cout_h (Printf.sprintf "#endif");
    close_out cout_h);
  close_out cout_c;
  close_out cout_ml
