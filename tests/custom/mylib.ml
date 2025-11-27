(* generated using generator.exe and camlid *)
type myptr
external of_int:
  (int [@untagged]) ->
  myptr
  = "camlid_stub_of_int_byte" "camlid_stub_of_int"
external to_int:
  myptr ->
  (int [@untagged])
  = "camlid_stub_to_int_byte" "camlid_stub_to_int"