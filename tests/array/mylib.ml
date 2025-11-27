(* generated using generator.exe and camlid *)
type camlid_int1 = int
type camlid_array = camlid_int1 array
type camlid_array1 = camlid_int1 array
type camlid_array2 = camlid_int1 array
type camlid_array3 = camlid_int1 array
type camlid_int = int
type camlid_array4 = camlid_int1 array
external f_output_input: camlid_array -> camlid_array = "camlid_stub_f"
external f_output: unit -> camlid_array1 = "camlid_stub_f1"
external f_input_output: camlid_array2 -> camlid_array2 = "camlid_stub_f2"
external f_input: camlid_array3 -> unit = "camlid_stub_f21"
external f4:
  (camlid_int [@untagged]) ->
  camlid_array4
  = "camlid_stub_f4_byte" "camlid_stub_f4"