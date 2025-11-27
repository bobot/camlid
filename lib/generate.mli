val to_file :
  ?in_header:bool ->
  ?prefix:string ->
  ?headers:string list ->
  ?definitions:string list ->
  string ->
  Expr.expr list ->
  unit
(** [to_file basename l] From the signature of the C functions [l], or more
    generally using {!Camlid.Expert} from the list of declaration to put in
    {e [basename].ml}, generates
    - {e basename.ml} which contains the externals
    - {e basename_stub.c} which contains the C stub for implementing the
      externals
    - {e basename_stub.h} which contains the includes, typedef and struct
      defined for the stub
    - {e basename_type.ml} which contains the alias, and type definition used

    If [~in_header] is false (default) the content of {e basename_stub.c} (and
    {e basename_type.ml}) are in {e basename_stub.c} (and {e basename.ml}
    respectively).

    @param headers list of headers to include
    @param definitions
      list of headers to include only in {e basename_stub.c}. It can be used to
      add definitions that use {e basename_stub.h}
    @param prefix
      set the prefix to use for the identifiers (C functions, structs,
      enumerations names and OCaml types and functions name) *)
