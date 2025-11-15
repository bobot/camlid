let command =
  Printf.ksprintf (fun s ->
      let code = Sys.command s in
      if code <> 0 then exit code)

let cat_and_compile filename =
  Printf.printf "  $ cat %s_stub.c\n%!" filename;
  command "cat %s_stub.c" filename;
  Printf.printf "\n  $ cat %s.ml\n%!" filename;
  command "cat %s.ml" filename;
  Printf.printf "\n  $ compile %s.c\n%!" filename;
  command "ocamlc -ccopt --warn-all -c %s_stub.c" filename;
  Printf.printf "\n  $ compile %s.ml\n%!" filename;
  command "ocamlc -c %s.ml" filename
