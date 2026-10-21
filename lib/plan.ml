(** The plan of a stub: the order of its steps and the values registered with
    the GC. {!schedule} makes every decision of safety, the printer
    ({!Expert.code_c_fun}) only translates the steps into C.

    [schedule] is written and proved in Why3 ([proof/plan.mlw]) and extracted in
    [plan_gen.ml]: the plans it gives are memory safe in the model of
    [proof/plan.mlw], provided that the C templates of the types match their
    contract. The parameters are numbered by their position. *)

include Plan_gen

(** The positions are OCaml integers ([OInt.t] in the proof) *)
type nonrec step = int step

type nonrec plan = int plan

let pure = { alloc = false; raising = false }

let message ~name = function
  | Bad_call ->
      Printf.sprintf
        "Camlid: in %s, a parameter of the call is not a parameter of the \
         function"
        name
  | Input_output_owned ->
      Printf.sprintf
        "Camlid: in %s, a parameter can't be both an input and an output when \
         its C value is owned by the OCaml value"
        name
  | Owned_unused ->
      Printf.sprintf
        "Camlid: in %s, a C value owned by OCaml is neither an input nor an \
         output: it would leak"
        name
  | Raising_owning_input ->
      Printf.sprintf
        "Camlid: in %s, the conversion of an input with a destructor can \
         raise: use a check"
        name
  | Raising_output ->
      Printf.sprintf
        "Camlid: in %s, the conversion of an output can raise: use a check" name
  | Lock_ml_heap ->
      Printf.sprintf
        "Camlid: the runtime lock can't be released in %s, an argument of the \
         C function points into the OCaml heap"
        name
  | Too_many_params -> Printf.sprintf "Camlid: %s has too many parameters" name
  | Not_noalloc ->
      Printf.sprintf
        "Camlid: %s can't be noalloc, a conversion allocates or the function \
         has many results, a check or releases the lock"
        name

let schedule ~name options params ~call =
  match schedule options params call with
  | Ok plan -> Stdlib.Ok plan
  | Error e -> Stdlib.Error (message ~name e)
