type typedef = {
  name : string;  (** unique base name *)
  descr : string;
  deps : typedef list;  (** typedef dependencies *)
  cty : unit Fmt.t;  (** print the c type *)
  mlty : unit Fmt.t;  (** print the ocaml type *)
  mlname : string;  (** ml name *)
  c2ml : funpar;  (** code of c2ml with declaration of the formals *)
  ml2c : funpar;  (** code of ml2c with declaration of the formals *)
  init : funpar;  (** code of init with declaration of the formals *)
  init_expr : funpar;  (** expression initialization *)
  extra_defs : unit Fmt.t;
      (** printed after declarations and before its definition *)
}

and funpar = { pp : unit Fmt.t; params : (string * typedef) list }
(** Allows for a code to depend on additional formal parameters *)

let nop = { pp = Fmt.nop; params = [] }
let any s = { pp = Fmt.any s; params = [] }

type fp = { fmt : 'a. ('a, Format.formatter, unit) format -> 'a }

let fp ?(params = []) pp =
  { pp = (fun fmt () -> pp { fmt = (fun p -> Fmt.pf fmt p) }); params }

let dpr = Format.dprintf

let dfp ?(params = []) =
  Format.kdprintf (fun k -> { pp = (fun fmt () -> k fmt); params })

let c2ml_of_name fmt name = Fmt.pf fmt "camlid_c2ml_%s" name

(** print c2ml function name of a typedef *)
let c2ml fmt td = c2ml_of_name fmt td.name

let ml2c_of_name fmt name = Fmt.pf fmt "camlid_ml2c_%s" name

(** print ml2c function name of a typedef *)
let ml2c fmt td = ml2c_of_name fmt td.name

let init_of_name fmt name = Fmt.pf fmt "camlid_init_%s" name

(** print c2ml function name of a typedef *)
let init fmt td = init_of_name fmt td.name

let cty_of_name fmt name = Fmt.pf fmt "camlid_c_%s" name

(** print c type alias of a typedef *)
let cty fmt td = cty_of_name fmt td.name

(** print ocaml type alias of a typedef *)
let mlty fmt td = Fmt.string fmt td.mlname

let init_expr fmt ty = ty.init_expr.pp fmt ()

type param = {
  input : bool; (* appears in ML parameters and converted before call *)
  output : bool; (* appears in ML results and converted after call *)
  used_in_call : bool; (* appears in the stubbed C call parameters *)
  pty : typedef;
  funpars : (string * param) List.t;
  pname : string;
}

type result = {
  routput : bool; (* appears in ML results and converted after call *)
  rty : typedef;
}

type func = { fname : string; params : param list; result : result option }

let stub_name fmt f = Fmt.pf fmt "camlid_fun_%s" f.fname

type decl = Fun of func
type conf = decl list

module C2ML = struct
  (** name of the destination ML value *)
  let v = "v"

  (** name of the source C value *)
  let c = "c"

  let call ~v ~c fmt ty = Fmt.pf fmt "%a(%t,%t)" c2ml ty v c
end

module ML2C = struct
  (** name of the destination value*)
  let v = "v"

  (** name of the source C value *)
  let c = "c"

  let call ~v ~c fmt ty = Fmt.pf fmt "%a(%t,%t)" ml2c ty c v
end

module INIT = struct
  (** name of the C value to initilizat *)
  let c = "c"

  let call ~c fmt ty = Fmt.pf fmt "%a(%t)" init ty c
end
