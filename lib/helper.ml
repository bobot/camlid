open Expr
open Type
open Expert

let int = int
let func = func
let ptr_ref = ptr_ref

let output_array ?input name ty =
  let a_len = array_length ty in
  let a = array_ptr_of_array_length ty a_len in
  let len_ptr = length_ptr_of_array_length ty a_len in
  let io_a_len =
    simple_param ?input ~output:true ~used_in_call:false a_len name
  in
  let a =
    ignored a (name ^ "_a") ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  let len_ptr =
    ignored len_ptr (name ^ "_len")
      ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  (io_a_len, a, len_ptr)

let input_array ?output name ty =
  let a_len = array_length ty in
  let a = array_ptr_of_array_length ty a_len in
  let len = length_of_array_length ty a_len in
  let io_a_len =
    simple_param ~input:true ?output ~used_in_call:false a_len name
  in
  let a =
    ignored a (name ^ "_a") ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  let len =
    ignored len (name ^ "_len")
      ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  (io_a_len, a, len)

let abstract ?get ?set ?internal ~ml ~c () : typedef =
  let descr = Printf.sprintf "abstract tag for type \"%s\"" c in
  let cty = typedef "abstract" "%s" c in
  let icty =
    match internal with
    | None -> cty
    | Some internal -> typedef "abstract_intern" "%s" internal
  in
  let get =
    Option.map
      (fun get ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { get = declare_existing get [ c; i ]; c; i })
      get
  in
  let set =
    Option.map
      (fun set ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { set = declare_existing set [ i; c ]; c; i })
      set
  in
  Expert.abstract ?set ?get ~icty ~descr ~cty ~ml ()

let custom = Expert.custom
let inout = Expert.inout
let input = Expert.input
let output = Expert.output
let ignored = Expert.ignored
