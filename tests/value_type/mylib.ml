(* generated using generator.exe and camlid *)
type vec
type zeros
external vec_range:
  (int [@untagged]) ->
  vec
  = "camlid_stub_vec_range_byte" "camlid_stub_vec_range"
external vec_push:
  vec ->
  (int [@untagged]) ->
  unit
  = "camlid_stub_vec_push_byte" "camlid_stub_vec_push" [@@noalloc]
external vec_sum:
  vec ->
  (int [@untagged])
  = "camlid_stub_vec_sum_byte" "camlid_stub_vec_sum" [@@noalloc]
external vec_ranges:
  (int [@untagged]) ->
  (vec array)
  = "camlid_stub_vec_ranges_byte" "camlid_stub_vec_ranges"
external vec_total:
  (vec array) ->
  (int [@untagged])
  = "camlid_stub_vec_total_byte" "camlid_stub_vec_total"
external vec_global: unit -> vec = "camlid_stub_vec_global"
external zeros:
  size:(int [@untagged]) ->
  zeros
  = "camlid_stub_zeros_byte" "camlid_stub_zeros"
external zeros_len:
  zeros ->
  (int [@untagged])
  = "camlid_stub_zeros_len_byte" "camlid_stub_zeros_len" [@@noalloc]
external vec_live:
  unit -> (int [@untagged])
  = "camlid_stub_vec_live_byte" "camlid_stub_vec_live" [@@noalloc]
