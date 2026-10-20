open Camlid
open Helper

let color =
  enum ~c:"enum color" "color"
    [ ("Red", "RED"); ("Green", "GREEN"); ("Blue", "BLUE") ]

let perm =
  flags "perm" [ ("Read", "F_READ"); ("Write", "F_WRITE"); ("Exec", "F_EXEC") ]

let () =
  Generate.to_file ~headers:[ "lib.h" ] "mylib"
    [
      func "next_color" [ input color ] ~result:color;
      func "bad_color" [] ~result:color;
      func "flags_add_exec" [ input perm ] ~result:perm;
      func "non_empty"
        [ input (nullable (string_nt ())) ]
        ~result:(nullable (string_nt ~owned:false ()));
    ]
