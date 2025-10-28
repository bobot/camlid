type var = string
type sign = Unsigned | Signed

type typedef = {
  name : string;  (** unique base name *)
  descr : string;
  deps : typedef list;  (** typedef dependencies *)
  cty : unit Fmt.t;  (** print the c type *)
  mlty : unit Fmt.t;  (** print the ocaml type *)
  mlname : string;  (** ml name *)
  c2ml : unit Fmt.t;  (** code of c2ml *)
  ml2c : unit Fmt.t;  (** code of ml2c *)
  init : unit Fmt.t;  (** code of init *)
  init_expr : unit Fmt.t;  (** expression initialization *)
}

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

type param = {
  input : bool; (* appears in ML parameters and converted before call *)
  output : bool; (* appears in ML results and converted after call *)
  pty : typedef;
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
