(* generated using generator.exe and camlid *)
type vec
external vec_dispose : vec -> unit = "camlid_vec_dispose" [@@noalloc]
type camlid_string_nt = string
exception My_error of string
let () = Callback.register_exception "Mylib.My_error" (My_error "")
type status = | Ok | Empty
external checked_div:
  (int [@untagged]) ->
  (int [@untagged]) ->
  (int [@untagged])
  = "camlid_stub_checked_div_byte" "camlid_stub_checked_div"
external make:
  (int [@untagged]) ->
  vec * camlid_string_nt
  = "camlid_stub_make_byte" "camlid_stub_make"
external slow_sum:
  (int [@untagged]) ->
  (int [@untagged])
  = "camlid_stub_slow_sum_byte" "camlid_stub_slow_sum"
external vec_signal_next_init:
  unit -> unit
  = "camlid_stub_vec_signal_next_init" [@@noalloc]
external vec_unlocked:
  (int [@untagged]) ->
  vec
  = "camlid_stub_vec_unlocked_byte" "camlid_stub_vec_unlocked"
external vec_live:
  unit -> (int [@untagged])
  = "camlid_stub_vec_live_byte" "camlid_stub_vec_live" [@@noalloc]
external vec_length:
  vec ->
  (int [@untagged])
  = "camlid_stub_vec_length_byte" "camlid_stub_vec_length"
external vec_status:
  (int [@untagged]) ->
  vec * status
  = "camlid_stub_vec_status_byte" "camlid_stub_vec_status"
external vec_live_alloc:
  unit -> (int [@untagged])
  = "camlid_stub_vec_live_alloc_byte" "camlid_stub_vec_live_alloc"
