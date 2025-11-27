(* generated using generator.exe and camlid *)
type result = | Data of int | Error of int
external f:
  (int [@untagged]) ->
  result
  = "camlid_stub_f_byte" "camlid_stub_f"