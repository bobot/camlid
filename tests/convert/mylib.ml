(* generated using generator.exe and camlid *)
type camlid_int = int
type result = | Data of camlid_int | Error of camlid_int
external f: camlid_int -> result = "camlid_stub_f"