open Expr
open Type
open Expert

type code = Expr.code
type expr = Expr.expr

let expr = expr
let pp_def = pp_def
let typedef = typedef

let simple_param ?binds ?input ?output ?used_in_call ?name pty =
  fst (simple_param ?binds ?input ?output ?used_in_call ?name pty)

let input ?used_in_call ?binds = simple_param ?used_in_call ?binds ~input:true
let output ?used_in_call ?binds = simple_param ?used_in_call ?binds ~output:true

let inout ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:true ~output:true

let ignored ?used_in_call ?binds =
  simple_param ?used_in_call ?binds ~input:false ~output:false

let int : typedef =
  builtin_mltypes "int" ~c_type:"intptr_t" ~c2ml:"Val_long" ~ml2c:"Long_val"
    ~unbox_attribute:Untagged

let int_trunc : typedef =
  builtin_mltypes "int" ~u_type:"intptr_t" ~c_type:"int" ~c2ml:"Val_int"
    ~ml2c:"Int_val" ~u2c:"(int)" ~c2u:"(intptr_t)" ~unbox_attribute:Untagged

let double : typedef =
  builtin_mltypes "float" ~c_type:"double" ~c2ml:"caml_copy_double"
    ~ml2c:"Double_val" ~unbox_attribute:Unboxed

let int32 : typedef =
  builtin_mltypes "int32" ~c_type:"int32_t" ~c2ml:"caml_copy_int32"
    ~ml2c:"Int32_val" ~unbox_attribute:Unboxed

let int64 : typedef =
  builtin_mltypes "int64" ~c_type:"int64_t" ~c2ml:"caml_copy_int64"
    ~ml2c:"Int64_val" ~unbox_attribute:Unboxed

let nativeint : typedef =
  builtin_mltypes "nativeint" ~c_type:"intptr_t" ~c2ml:"caml_copy_nativeint"
    ~ml2c:"Nativeint_val" ~unbox_attribute:Unboxed

let bool : typedef =
  builtin_mltypes "bool" ~c_type:"int" ~c2ml:"Val_bool" ~ml2c:"Bool_val"
    ~unbox_attribute:Untagged

let string_nt = Expert.string_nt
let ptr_ref = ptr_ref

let func_id ~ml ?result ?ignored_result fid params =
  (* C function declaration *)
  match (result, ignored_result) with
  | Some _, Some _ ->
      failwith "Camlid.Helper.func: can't set both result and ignored_result"
  | Some rty, None ->
      let rc = Var.mk "res" (e_def rty.cty) in
      let rv' = Var.mk "vres" (expr "value") in
      let bind' code =
        Expr.binds [ (rty.c, e_addr rc); (rty.v, e_addr rv') ] code
      in
      let routput =
        match rty.conv with
        | Boxed { c2ml; ml2c = _ } ->
            POBoxed { ml = rv'; c2ml = bind' c2ml; pmlty = rty.mlty }
        | Unboxable
            {
              unbox_attribute;
              uty;
              ml2u = _;
              u2ml;
              u2c = _;
              c2u;
              u;
              ml2c = _;
              c2ml;
            } ->
            let ru = Var.mk "ures" (e_def uty) in
            let bind' code =
              Expr.binds
                [ (u, e_addr ru); (rty.c, e_addr rc); (rty.v, e_addr rv') ]
                code
            in
            POUnboxable
              {
                unbox_attribute;
                ml = rv';
                u = ru;
                c2u = bind' c2u;
                u2ml = bind' u2ml;
                c2ml = bind' c2ml;
                pmlty = rty.mlty;
              }
      in
      print_ml_fun fid ~mlname:ml ~params
        ~result:{ routput; rc; rfree = Option.map bind' rty.free }
  | None, Some rty ->
      let rc = Var.mk "res" (e_def rty.cty) in
      let bind' code = Expr.binds [ (rty.c, e_addr rc) ] code in
      print_ml_fun fid ~mlname:ml ~params
        ~result:{ routput = PONone; rc; rfree = Option.map bind' rty.free }
  | None, None -> print_ml_fun fid ~mlname:ml ~params

let func ?(declare = false) ?ml ?result ?ignored_result fname params =
  let ml = Option.value ~default:fname ml in
  let fid =
    let vars_used_in_calls =
      List.filter_map (fun p -> Option.map fst p.pused_in_call) params
    in
    if declare then
      declare_existing
        ?result:(Option.map (fun rty -> e_def rty.cty) result)
        fname vars_used_in_calls
    else existing fname vars_used_in_calls
  in
  func_id ~ml ?result ?ignored_result fid params

let func_in ?ml ?result fname inputs =
  func ?ml ?result fname (List.map (fun ty -> input ty ~name:"v") inputs)

let input_array ?owned ?(output = false) ?(input = true) ?(name = "array") ty =
  let a_len = array_length ?owned ty in
  let io_a_len, _ = Expert.simple_param ~input ~output a_len ~name in

  let a = t_field ty io_a_len in
  let len_ptr = len_field io_a_len |> use_new_param_only_in_call in
  (a, len_ptr)

let output_array ?owned ?(output = true) ?(input = false) ?name ty =
  let a, len_ptr = input_array ?owned ~output ~input ?name ty in
  (deref_in_call a, deref_in_call len_ptr)

let fixed_length_array ?init ?owned ?(input = false) ?(output = true)
    ?(len_used_in_call = false) ?(name = "array") ty =
  let len, len_pc =
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call int
      ~name:(name ^ "_len")
  in
  let a_len =
    simple_param ~input ~output (array ?init ?owned ~len:len_pc ty) ~name
  in
  (a_len, len)

let input_string ?owned ?(output = false) ?(name = "string") () =
  let a_len = string_length ?owned () in
  let io_a_len, _ = Expert.simple_param ~input:true ~output a_len ~name in
  let a = get_field (expr "char *") "t" io_a_len in
  let len_ptr = len_field io_a_len |> use_new_param_only_in_call in
  (deref_in_call a, deref_in_call len_ptr)

let output_string ?owned ?(output = false) ?name () =
  let a, len_ptr = input_string ?owned ~output ?name () in
  (a, len_ptr)

let fixed_length_string ?init ?owned ?(input = false) ?(output = true)
    ?(len_used_in_call = false) ?(name = "string") () =
  let len, len_pc =
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call int
      ~name:(name ^ "_len")
  in
  let a_len =
    simple_param ~input ~output (string_fixed_length ?init ?owned len_pc) ~name
  in
  (a_len, len)

let abstract ?initialize ?get ?set ?internal ~ml ~c () : typedef =
  let cty = typedef "abstract" "%s" c in
  let icty =
    match internal with
    | None -> cty
    | Some internal -> typedef "abstract_intern" "%s" internal
  in
  let initialize =
    Option.map
      (fun initialize ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        { initialize = declare_existing initialize [ c ]; c })
      initialize
  in
  let get =
    Option.map
      (fun get ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        let i = Var.mk "i" (expr "%a *" pp_def icty) in
        { get = declare_existing get [ c; i ]; c; i })
      get
  in
  let set =
    Option.map
      (fun set ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        let i = Var.mk "i" (expr "%a *" pp_def icty) in
        { set = declare_existing set [ i; c ]; c; i })
      set
  in
  Expert.abstract ?initialize ?set ?get ~icty ~cty ~ml ()

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

let algdata ml_type l =
  let t = AlgData.algdata ml_type l in
  let others = List.map (fun c -> c.AlgData.smart_constructor) t.constrs in
  { t.ty with cty = DImplicit (t.ty.cty, others) }
