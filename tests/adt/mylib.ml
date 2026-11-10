(* generated using generator.exe and camlid *)
type t = | T1 of int*float | T2 | T3 of int
type camlid_string_nt = string
type name = | Anonymous | Named of camlid_string_nt
external f_print: t -> unit = "camlid_stub_f_print"
external f_hello: name -> unit = "camlid_stub_f_hello"
external f_make:
  (int [@untagged]) ->
  name
  = "camlid_stub_f_make_byte" "camlid_stub_f_make"
