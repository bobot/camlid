(* generated using generator.exe and camlid *)
type camlid_int = int
type result = | Data of camlid_int | Error of camlid_int
external f:
  (camlid_int [@untagged]) ->
  result
  = "camlid_stub_f_byte" "camlid_stub_f"