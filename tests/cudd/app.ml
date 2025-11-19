(** The implementation of cudd is a stub *)
let ctx = Mylib.init ()

let a = Mylib.bdd_var ctx 0
let b = Mylib.bdd_var ctx 1
let c = Mylib.bdd_or ctx a b

let () =
  match Mylib.inspect ctx c with
  | False -> Format.printf "False@."
  | True -> Format.printf "True@."
  | Ifte (v, _, _) -> Format.printf "Ifte(%i,_,_)@." v
