open Expr
open Type
open Expert

let simple_param ?input ?output ?name pty =
  fst (simple_param ~binds:[] ?input ?output ~used_in_call:true ?name pty)

let input = simple_param ~input:true ~output:false
let output = simple_param ~output:true ~input:false
let inout = simple_param ~input:true ~output:true
let ignored = simple_param ~input:false ~output:false

let int : typedef =
  builtin_mltypes "int" ~c_type:"intptr_t" ~c2ml:"Val_long" ~ml2c:"Long_val"
    ~unbox_attribute:Untagged

let size_t : typedef =
  builtin_mltypes "int" ~c_type:"size_t" ~c2ml:"Val_long"
    ~ml2c:"(size_t)Long_val" ~unbox_attribute:Untagged

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
      print_ml_fun fid ~mlname:ml ~params ~result:(Expert.simple_result rty)
  | None, Some rty ->
      let rc = Var.mk "res" rty.cty.cty in
      let bind' code = Expr.binds [ (rty.cty.c, e_addr rc) ] code in
      print_ml_fun fid ~mlname:ml ~params
        ~result:{ routput = PONone; rc; rfree = Option.map bind' rty.cty.free }
  | None, None -> print_ml_fun fid ~mlname:ml ~params

let func ?ml ?result ?ignored_result fname params =
  let ml = Option.value ~default:fname ml in
  let fid =
    let vars_used_in_calls =
      List.filter_map (fun p -> Option.map fst p.pused_in_call) params
    in
    existing fname vars_used_in_calls
  in
  func_id ~ml ?result ?ignored_result fid params

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
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call size_t
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
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call size_t
      ~name:(name ^ "_len")
  in
  let a_len =
    simple_param ~input ~output (string_fixed_length ?init ?owned len_pc) ~name
  in
  (a_len, len)

let file_struct =
  let id = ID.mk "file_s" in
  toplevel id "struct %a { char* t; size_t len; FILE *file;};@." pp_id id

let string_as_FILE_ptr =
  let cty = typedef "file" "struct %a" pp_def file_struct in
  let v = Var.mk "v" (expr "value") in
  let c = Var.mk "c" (expr "%a" pp_def cty) in
  let v' = Var.mk "v" (expr "value *") in
  let c' = Var.mk "c" (expr "%a *" pp_def cty) in
  let malloc { fmt } =
    fmt "%a->file = open_memstream(&(%a->t),&(%a->len));" pp_var c' pp_var c'
      pp_var c'
  in
  {
    mlty = expr "string";
    mlname = None;
    conv =
      Boxed
        {
          ml2c =
            call_codef "ml2c"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                malloc { fmt };
                fmt "fwrite(String_val(*%a),caml_string_length(*%a),1,%a->file)"
                  pp_var v' pp_var v' pp_var c');
          c2ml =
            call_codef "c2ml"
              [ (v', e_addr v); (c', e_addr c) ]
              (fun { fmt } ->
                fmt "fflush(%a->file);@ " pp_var c';
                fmt "fflush(stdout);@ ";
                fmt "*%a = caml_alloc_string(%a->len);@ " pp_var v' pp_var c';
                fmt "memcpy(&Byte(*%a,0),%a->t,%a->len);" pp_var v' pp_var c'
                  pp_var c');
        };
    cty =
      {
        cty = e_def cty;
        init =
          Some (call_codef "init" [ (v', e_addr v); (c', e_addr c) ] malloc);
        init_expr = expr "((%a) { 0 })" pp_def cty;
        free = Some (expr "fclose(%a.file);" pp_var c);
        in_call = Some (expr "%a.file" pp_var c);
        c;
      };
    v;
  }

let input_value ml = input (value ml)
let output_value ml = output (value ml) |> deref_in_call

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
        { initialize = existing initialize [ c ]; c })
      initialize
  in
  let get =
    Option.map
      (fun get ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        let i = Var.mk "i" (expr "%a *" pp_def icty) in
        { get = existing get [ c; i ]; c; i })
      get
  in
  let set =
    Option.map
      (fun set ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        let i = Var.mk "i" (expr "%a *" pp_def icty) in
        { set = existing set [ i; c ]; c; i })
      set
  in
  Expert.abstract ?initialize ?set ?get ~icty ~cty ~ml ()

(** Encapsulate a c type into an custom ml type *)
let custom ?initialize ?finalize ?hash ?compare ?get ?set ?internal ~ml ~c () =
  let cty = expr "%s" c in
  let icty = match internal with None -> cty | Some c -> expr "%s" c in
  let get = Option.map (mk_get ~icty ~cty) get in
  let set = Option.map (mk_set ~icty ~cty) set in
  let finalize = Option.map (mk_finalize ~icty) finalize in
  let hash = Option.map (mk_hash ~icty) hash in
  let compare = Option.map (mk_compare ~icty) compare in
  let initialize = Option.map (mk_initialize ~cty) initialize in
  custom ?initialize ?finalize ?hash ?compare ?get ?set ~ml ~icty ~cty ()

let custom_ptr ?initialize ?finalize ?hash ?compare ?malloc ~ml ~c () =
  let cty = expr "%s" c in
  let icty = cty in
  let finalize = Option.map (mk_finalize ~icty) finalize in
  let hash = Option.map (mk_hash ~icty) hash in
  let compare = Option.map (mk_compare ~icty) compare in
  let initialize = Option.map (mk_initialize ~cty) initialize in
  custom_ptr ?initialize ?finalize ?hash ?compare ?malloc ~ml ~cty ()

let algdata ml_type l =
  let t = AlgData.algdata ml_type l in
  let others = List.map (fun c -> c.AlgData.smart_constructor) t.constrs in
  let cty = typedef "algdata" "%a" pp_expr t.ty.cty.cty in
  let cty = Expr.dimplicit cty others in
  { t.ty with cty = { t.ty.cty with cty = e_def cty } }

let module_ name l =
  expr "@[<hv 3>module %s = struct@ %a@ end@]" name Fmt.(list ~sep:sp pp_expr) l

let ml_alias name typedef = expr "@[type %s = %a@]" name pp_expr typedef.mlty

let copy typedef ?vars ?exprs string =
  Expert.copy ~copy:(mk_copy ~cty:typedef.cty.cty ?exprs ?vars string) typedef

let ret_option_if typedef =
  let status, v_status = Expert.simple_param bool in
  let status = deref_in_call status in
  (status, Expert.ret_option_if (e_var v_status) typedef)

let get_expression ~name ty s =
  Expert.get_expression ~mlname:name ty (expr "%s" s)

let map_param_in_call ?(name = "arg") ~ty param fmt =
  Expert.map_param_in_call ~name
    (fun _ e -> (expr "%s" ty, expr "%(%a%)" fmt pp_expr e))
    param

let do_nothing ?result ?ignored_result ml =
  func_id ~ml ?result ?ignored_result (expr "")

let convert ?c_to_mlc ?mlc_to_c ?(using = []) ~mlc ~c () =
  let mk =
    Option.map (fun name ->
        Expert.mk_converter ~src:c ~dst:mlc.cty name ~vars:(fun ~dst ~src ->
            [ dst; src ] @ using))
  in
  Expert.convert ?c_to_mlc:(mk c_to_mlc) ?mlc_to_c:(mk mlc_to_c) ~mlc ~c ()
