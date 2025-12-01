type defined
type code
type expr
type kind = C | ML | H
type id
type var = private { id : int; name : string; ty : expr }

val expr : ('a, Format.formatter, unit, expr) format4 -> 'a
val pp_expr : Format.formatter -> expr -> unit
val binds : (var * expr) list -> code -> code
val dimplicit : defined -> code list -> defined
val implicit : code -> code list -> code

module ID : sig
  type t = id

  val hash : t -> int
  val equal : 'a -> 'a -> bool
  val compare : t -> t -> int
  val mk : ?keep_name:bool -> string -> t

  module H : Hashtbl.S with type key = t
  module S : Set.S with type elt = t
end

module Var : sig
  type t = var

  val hash : t -> int
  val equal : 'a -> 'a -> bool
  val compare : t -> t -> int
  val mk : string -> expr -> var

  module H : Hashtbl.S with type key = t
  module S : Set.S with type elt = t
end

val def_of_code : code -> defined
val name_of_def : defined -> string
val pp_id : Format.formatter -> ID.t -> unit
val pp_def : Format.formatter -> defined -> unit
val e_def : defined -> expr
val pp_var : Format.formatter -> Var.t -> unit
val e_var : Var.t -> expr
val e_addr : Var.t -> expr
val e_deref : Var.t -> expr
val pp_call : Format.formatter -> code * (Var.t * expr) list -> unit
val pp_calli : Format.formatter -> code * (Var.t * expr) list -> unit
val pp_call_ret : Format.formatter -> expr * code * (Var.t * expr) list -> unit
val def : ?kind:kind -> id -> unit Fmt.t -> defined

val ddef :
  ?kind:kind -> id -> ('a, Format.formatter, unit, defined) format4 -> 'a

val mk : ?kind:kind -> ?params:var list -> id -> unit Fmt.t -> code

type fp = { fmt : 'a. ('a, Format.formatter, unit) format -> 'a }

val fp : ?kind:kind -> ?params:var list -> id -> (fp -> unit) -> code

val dfp :
  ?kind:kind ->
  ?params:var list ->
  id ->
  ('a, Format.formatter, unit, code) format4 ->
  'a

type env

val toplevel_callable :
  ?kind:kind -> id -> ('a, Format.formatter, unit, code) format4 -> 'a

val toplevel :
  ?kind:kind -> id -> ('a, Format.formatter, unit, defined) format4 -> 'a

val final_print :
  prefix:string ->
  ml:out_channel ->
  c:out_channel ->
  h:out_channel ->
  kind ->
  expr ->
  unit

val codef :
  ?inline:bool ->
  ?kind:kind ->
  ?params:Var.S.elt list ->
  ?keep_name:bool ->
  ?locals:Var.S.elt list ->
  ?ovars:Var.t list ->
  ?ret:expr ->
  ?doc:unit Fmt.t ->
  string ->
  (fp -> unit) ->
  code

val codefo :
  ?kind:kind ->
  ?params:Var.S.elt list ->
  ?keep_name:bool ->
  ?locals:Var.S.elt list ->
  ?ovars:Var.t list ->
  ?ret:expr ->
  ?doc:unit Fmt.t ->
  string ->
  (fp -> unit) ->
  code option

val code :
  ?inline:bool ->
  ?kind:kind ->
  ?params:Var.S.elt list ->
  ?keep_name:bool ->
  ?locals:Var.S.elt list ->
  ?ovars:Var.t list ->
  ?ret:expr ->
  ?doc:unit Fmt.t ->
  string ->
  ('a, Format.formatter, unit, code) format4 ->
  'a

val codeo :
  ?kind:kind ->
  ?params:Var.S.elt list ->
  ?keep_name:bool ->
  ?locals:Var.S.elt list ->
  ?ovars:Var.t list ->
  ?ret:expr ->
  ?doc:unit Fmt.t ->
  string ->
  ('a, Format.formatter, unit, code option) format4 ->
  'a
