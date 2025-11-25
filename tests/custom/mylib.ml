(* generated using generator.exe and camlid *)
type camlid_int = int
type myptr
external of_int:
  (camlid_int [@untagged]) ->
  myptr
  = "camlid_stub_of_int_byte" "camlid_stub_of_int"
external to_int:
  myptr ->
  (camlid_int [@untagged])
  = "camlid_stub_to_int_byte" "camlid_stub_to_int"