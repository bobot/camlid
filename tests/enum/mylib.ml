(* generated using generator.exe and camlid *)
type color = | Red | Green | Blue
type perm = | Read | Write | Exec
type camlid_string_nt = string
external next_color: color -> color = "camlid_stub_next_color"
external bad_color: unit -> color = "camlid_stub_bad_color"
external flags_add_exec:
  perm list ->
  perm list
  = "camlid_stub_flags_add_exec"
external non_empty:
  (camlid_string_nt option) ->
  (camlid_string_nt option)
  = "camlid_stub_non_empty"
