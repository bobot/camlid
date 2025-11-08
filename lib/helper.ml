open Expr
open Type
open Expert

type code = Expr.code
type expr = Expr.expr

let expr = expr
let pp_code = pp_code
let typedef = typedef

let int : typedef =
  builtin_mltypes ~ml_type:"int" ~c_type:"intnat" ~c2ml:"Val_long"
    ~ml2c:"Long_val"

let int_trunc : typedef =
  builtin_mltypes ~ml_type:"int" ~c_type:"int" ~c2ml:"Val_int" ~ml2c:"Int_val"

let double : typedef =
  builtin_mltypes ~ml_type:"float" ~c_type:"double" ~c2ml:"caml_copy_double"
    ~ml2c:"Double_val"

let int32 : typedef =
  builtin_mltypes ~ml_type:"int32" ~c_type:"int32_t" ~c2ml:"caml_copy_int32"
    ~ml2c:"Int32_val"

let int64 : typedef =
  builtin_mltypes ~ml_type:"int64" ~c_type:"int64_t" ~c2ml:"caml_copy_int64"
    ~ml2c:"Int64_val"

let nativeint : typedef =
  builtin_mltypes ~ml_type:"nativeint" ~c_type:"intnat"
    ~c2ml:"caml_copy_nativeint" ~ml2c:"Nativeint_val"

let bool : typedef =
  builtin_mltypes ~ml_type:"bool" ~c_type:"int" ~c2ml:"Val_bool"
    ~ml2c:"Bool_val"

let ptr_ref = ptr_ref

let func_id ~ml ?result ?ignored_result fid params =
  (* C function declaration *)
  match (result, ignored_result) with
  | Some _, Some _ ->
      failwith "Camlid.Helper.func: can't set both result and ignored_result"
  | Some rty, None ->
      print_ml_fun
        {
          fid;
          mlname = ml;
          params;
          result =
            Some
              {
                rty;
                routput = true;
                rc = Var.mk "res" (e_code rty.cty);
                binds = [];
              };
        }
  | None, Some rty ->
      print_ml_fun
        {
          fid;
          mlname = ml;
          params;
          result =
            Some
              {
                rty;
                routput = false;
                rc = Var.mk "res" (e_code rty.cty);
                binds = [];
              };
        }
  | None, None -> print_ml_fun { fid; mlname = ml; params; result = None }

let func ?(declare = false) ?ml ?result ?ignored_result fname params =
  let ml = Option.value ~default:fname ml in
  let fid =
    let used_in_calls = List.filter (fun p -> p.used_in_call) params in
    let vars_used_in_calls = List.map (fun p -> p.pc) used_in_calls in
    if declare then
      declare_existing
        ?result:(Option.map (fun rty -> e_code rty.cty) result)
        fname vars_used_in_calls
    else existing fname vars_used_in_calls
  in
  func_id ~ml ?result ?ignored_result fid params

let func_in ?ml ?result fname inputs =
  func ?ml ?result fname (List.map (fun ty -> input ty "v") inputs)

let output_array ?(input = false) name ty =
  let a_len = array_length ty in
  let a = array_ptr_of_array_length ty a_len in
  let len_ptr = length_ptr_of_array_length ty a_len in
  let io_a_len =
    simple_param ~input ~output:true ~used_in_call:false a_len name
  in
  let a =
    ignored a (name ^ "_a") ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  let len_ptr =
    ignored len_ptr (name ^ "_len")
      ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  (io_a_len, a, len_ptr)

let input_array ?(output = false) name ty =
  let a_len = array_length ty in
  let a = array_ptr_of_array_length ty a_len in
  let len = length_of_array_length ty a_len in
  let io_a_len =
    simple_param ~input:true ~output ~used_in_call:false a_len name
  in
  let a =
    ignored a (name ^ "_a") ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  let len =
    ignored len (name ^ "_len")
      ~binds:[ (a_len.c, expr "&%a" pp_var io_a_len.pc) ]
  in
  (io_a_len, a, len)

let output_set_length_array ?(input = false) ?(output = true)
    ?(len_used_in_call = false) name ty =
  let len = Expert.input ~used_in_call:len_used_in_call int (name ^ "_len") in
  let a_len = simple_param ~input ~output (array ~len:len.pc ty) name in
  (a_len, len)

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

(** Encapsulate a c type into an custom ml type *)
let custom ?initialize ?finalize ?finalize_ptr ?hash ?compare ?get ?set
    ?internal ~ml ~c () =
  let cty = typedef "custom" "%s" c in
  let icty =
    match internal with None -> cty | Some c -> typedef "custom_intern" "%s" c
  in
  let get = Option.map (mk_get ~icty ~cty) get in
  let set = Option.map (mk_set ~icty ~cty) set in
  let finalize = Option.map (mk_finalize ~icty) finalize in
  let finalize_ptr = Option.map (mk_finalize_ptr ~icty) finalize_ptr in
  let hash = Option.map (mk_hash ~icty) hash in
  let compare = Option.map (mk_compare ~icty) compare in
  let initialize = Option.map (mk_initialize ~cty) initialize in
  custom ?initialize ?finalize ?finalize_ptr ?hash ?compare ?get ?set ~ml ~icty
    ~cty ()

let inout = Expert.inout
let input = Expert.input
let output = Expert.output
let ignored = Expert.ignored
