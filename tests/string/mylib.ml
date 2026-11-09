(* generated using generator.exe and camlid *)
type camlid_string_nt = string
external f_in: camlid_string_nt -> unit = "camlid_stub_f_in"
external f_in2:
  camlid_string_nt ->
  camlid_string_nt ->
  unit
  = "camlid_stub_f_in2"
external f_in_prefix:
  camlid_string_nt ->
  unit
  = "camlid_stub_f_in_prefix" [@@noalloc]
external f_out: unit -> camlid_string_nt = "camlid_stub_f_out"
external f_in3:
  (int [@untagged]) ->
  string
  = "camlid_stub_f_in3_byte" "camlid_stub_f_in3"
external f_print_n:
  string ->
  (int [@untagged]) ->
  unit
  = "camlid_stub_f_print_n_byte" "camlid_stub_f_print_n"
external f_len: string -> unit = "camlid_stub_f_len" [@@noalloc]
external f_out_len: unit -> string = "camlid_stub_f_out_len"
