type code = Expr.code
type expr = Expr.expr

val expr : ('a, Format.formatter, unit, Expr.expr) format4 -> 'a
val pp_def : Format.formatter -> Expr.defined -> unit
val typedef : string -> ('a, Format.formatter, unit, Expr.defined) format4 -> 'a

val simple_param :
  ?binds:(Expr.var * Expr.expr) list ->
  ?input:bool ->
  ?output:bool ->
  ?used_in_call:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param

val input :
  ?used_in_call:bool ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?output:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param

val output :
  ?used_in_call:bool ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?input:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param

val inout :
  ?used_in_call:bool ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?name:string ->
  Type.typedef ->
  Type.param

val ignored :
  ?used_in_call:bool ->
  ?binds:(Expr.var * Expr.expr) list ->
  ?name:string ->
  Type.typedef ->
  Type.param

val int : Type.typedef
val int_trunc : Type.typedef
val double : Type.typedef
val int32 : Type.typedef
val int64 : Type.typedef
val nativeint : Type.typedef
val bool : Type.typedef
val string_nt : ?owned:bool -> unit -> Type.typedef
val ptr_ref : Type.typedef -> Type.typedef

val func_id :
  ml:string ->
  ?result:Type.typedef ->
  ?ignored_result:Type.typedef ->
  Expr.code ->
  Type.param list ->
  Expr.expr

val func :
  ?declare:bool ->
  ?ml:string ->
  ?result:Type.typedef ->
  ?ignored_result:Type.typedef ->
  string ->
  Type.param list ->
  Expr.expr

val func_in :
  ?ml:string -> ?result:Type.typedef -> string -> Type.typedef list -> Expr.expr

val input_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val output_array :
  ?owned:bool ->
  ?output:bool ->
  ?input:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val fixed_length_array :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  Type.typedef ->
  Type.param * Type.param

val input_string :
  ?owned:bool -> ?output:bool -> ?name:string -> unit -> Type.param * Type.param

val output_string :
  ?owned:bool -> ?output:bool -> ?name:string -> unit -> Type.param * Type.param

val fixed_length_string :
  ?init:bool ->
  ?owned:bool ->
  ?input:bool ->
  ?output:bool ->
  ?len_used_in_call:bool ->
  ?name:string ->
  unit ->
  Type.param * Type.param

val abstract :
  ?initialize:string ->
  ?get:string ->
  ?set:string ->
  ?internal:string ->
  ml:string ->
  c:string ->
  unit ->
  Type.typedef

val custom :
  ?initialize:string ->
  ?finalize:string ->
  ?finalize_ptr:string ->
  ?hash:string ->
  ?compare:string ->
  ?get:string ->
  ?set:string ->
  ?internal:string ->
  ml:string ->
  c:string ->
  unit ->
  Type.typedef

val algdata :
  string -> (string * (string * Type.typedef) list) list -> Type.typedef
