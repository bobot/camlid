(* generated using generator.exe and camlid *)
type camlid_int = int
type camlid_myptr
external of_int: camlid_int -> camlid_myptr = "camlid_stub_of_int"
external to_int: camlid_myptr -> camlid_int = "camlid_stub_to_int"
external lib_free: camlid_myptr -> unit = "camlid_stub_lib_free"