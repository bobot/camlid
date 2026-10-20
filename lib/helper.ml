open Expr
open Ty
open Expert

let simple_param ?input ?output ?name pty =
  fst (simple_param ~binds:[] ?input ?output ~used_in_call:true ?name pty)

let input = simple_param ~input:true ~output:false
let output = simple_param ~output:true ~input:false
let inout = simple_param ~input:true ~output:true
let ignored = simple_param ~input:false ~output:false

let int : mlc =
  builtin_mltypes "int" ~c_type:"intptr_t" ~c2ml:"Val_long" ~ml2c:"Long_val"
    ~unbox_attribute:Untagged ~unbox_version:(4, 3)

let size_t : mlc =
  builtin_mltypes "int" ~u_type:"intptr_t" ~c_type:"size_t" ~u2c:"(size_t)"
    ~ml2u:"Long_val" ~c2ml:"Val_long" ~ml2c:"(size_t)Long_val" ~c2u:"(intptr_t)"
    ~unbox_attribute:Untagged ~unbox_version:(4, 3)

let c_int : mlc =
  builtin_mltypes "int" ~u_type:"intptr_t" ~c_type:"int" ~c2ml:"Val_int"
    ~ml2c:"Int_val" ~u2c:"(int)" ~c2u:"(intptr_t)" ~unbox_attribute:Untagged
    ~unbox_version:(4, 3)

let double : mlc =
  builtin_mltypes "float" ~c_type:"double" ~c2ml:"caml_copy_double"
    ~ml2c:"Double_val" ~unbox_attribute:Unboxed ~unbox_version:(4, 3)

let int32 : mlc =
  builtin_mltypes "int32" ~c_type:"int32_t" ~c2ml:"caml_copy_int32"
    ~ml2c:"Int32_val" ~unbox_attribute:Unboxed ~unbox_version:(4, 3)

let int64 : mlc =
  builtin_mltypes "int64" ~c_type:"int64_t" ~c2ml:"caml_copy_int64"
    ~ml2c:"Int64_val" ~unbox_attribute:Unboxed ~unbox_version:(4, 3)

let nativeint : mlc =
  builtin_mltypes "nativeint" ~c_type:"intptr_t" ~c2ml:"caml_copy_nativeint"
    ~ml2c:"Nativeint_val" ~unbox_attribute:Unboxed ~unbox_version:(4, 3)

let bool : mlc =
  builtin_mltypes "bool" ~c_type:"int" ~c2ml:"Val_bool" ~ml2c:"Bool_val"
    ~unbox_attribute:Untagged ~unbox_version:(5, 2)

let string_nt = Expert.string_nt
let string_nt_prefix = Expert.string_nt_prefix
let ptr_ref = ptr_ref

let func_id ?release_lock ?error ~ml ?result ~call_params fid params =
  (* C function declaration *)
  print_ml_fun ?release_lock ?error ~call_params fid ~mlname:ml ~params ?result

type error = fname:string -> Var.t option -> Expert.error

let exception_ = declare_exception

let error ?exn ?msg ?(vars = []) is_error ~fname res =
  let args = Option.to_list res @ vars in
  let call f = expr "%a" pp_call (existing f args, []) in
  let msg =
    match msg with
    | None -> expr "%S" (fname ^ " failed")
    | Some msg -> call msg
  in
  { cond = call is_error; msg = Some msg; exn }

let func_res ?release_lock ?error ?ml ?result ?call_params fname params =
  let call_params = Option.value call_params ~default:params in
  let ml = Option.value ~default:fname ml in
  let fid =
    let vars_used_in_calls =
      List.filter_map (fun p -> Option.map fst p.pused_in_call) call_params
    in
    let pp_result fmt = function
      | None -> ()
      | Some r -> Fmt.pf fmt "%a = " pp_var r.rc
    in
    expr "%a%a;" pp_result result pp_call (existing fname vars_used_in_calls, [])
  in
  let error =
    Option.map (fun e -> e ~fname (Option.map (fun r -> r.rc) result)) error
  in
  func_id ?release_lock ?error ~ml ?result ~call_params fid params

let func ?release_lock ?error ?ml ?result ?ignored_result ?call_params fname
    params =
  let result =
    match (result, ignored_result) with
    | Some _, Some _ ->
        failwith "Camlid.Helper.func: can't set both result and ignored_result"
    | Some rty, None -> Some (Expert.simple_result rty)
    | None, Some { cty = { free = None; _ }; _ } when Option.is_none error ->
        None
    | None, Some rty ->
        let rc = Var.mk "res" rty.cty.cty in
        let bind' code = Expr.binds [ (rty.cty.c, e_addr rc) ] code in
        Some { routput = PONone; rc; rfree = Option.map bind' rty.cty.free }
    | None, None -> None
  in
  func_res ?release_lock ?error ?ml ?result ?call_params fname params

type with_length = { t : Ty.param; len : Ty.param }

let array_len ?owned ?(input = true) ?(output = false) ?(name = "array") ty =
  let a_len = array_length ?owned ty in
  let io_a_len, _ = Expert.simple_param ~input ~output a_len ~name in

  let a = t_field ty io_a_len in
  let len_ptr = len_field io_a_len |> use_new_param_only_in_call in
  { t = a; len = len_ptr }

let array_len_ref ?owned ?(input = false) ?(output = true) ?name ty =
  let a = array_len ?owned ~output ~input ?name ty in
  { t = deref_in_call a.t; len = deref_in_call a.len }

let fixed_length_array ?init ?owned ?(input = false) ?(output = true)
    ?(len_used_in_call = false) ?(name = "array") ty =
  let len, len_pc =
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call size_t
      ~name:(name ^ "_len")
  in
  let a_len =
    simple_param ~input ~output (array ?init ?owned ~len:len_pc ty) ~name
  in
  { t = a_len; len }

let string_len ?owned ?(input = true) ?(output = false) ?(name = "string") () =
  let a_len = string_length ?owned () in
  let io_a_len, _ = Expert.simple_param ~input ~output a_len ~name in
  let a = get_field (expr "char *") "t" io_a_len in
  let len_ptr = len_field io_a_len |> use_new_param_only_in_call in
  { t = a; len = len_ptr }

let string_len_ref ?owned ?(input = false) ?(output = true) ?(name = "string")
    () =
  let a = string_len ?owned ~input ~output ~name () in
  { t = deref_in_call a.t; len = deref_in_call a.len }

let fixed_length_string ?init ?owned ?(input = false) ?(output = true)
    ?(len_used_in_call = false) ?(name = "string") () =
  let len, len_pc =
    Expert.simple_param ~input:true ~used_in_call:len_used_in_call size_t
      ~name:(name ^ "_len")
  in
  let a_len =
    simple_param ~input ~output (string_fixed_length ?init ?owned len_pc) ~name
  in
  { t = a_len; len }

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
        free = Some (expr "fclose(%a.file); free(%a.t);" pp_var c pp_var c);
        in_call = Some (expr "%a.file" pp_var c);
        c;
      };
    v;
    owned_by_ml = false;
    ml_heap = false;
  }

let input_value ml = input (value ml)
let output_value ml = output (value ml) |> deref_in_call

let abstract ?init ~ml ~c () : mlc =
  let cty = typedef "abstract" "%s" c in
  let init =
    Option.map
      (fun init ->
        let c = Var.mk "c" (expr "%a *" pp_def cty) in
        { init = calli_existing init [ c ]; c })
      init
  in
  Expert.abstract ?init ~icty:(e_def cty) ~cty:(e_def cty) ~ml ()

(** Encapsulate a c type into an custom ml type *)
let custom ?init ?clear ?hash ?compare ?mem ~ml ~c () =
  let cty = expr "%s" c in
  let icty = cty in
  let clear = Option.map (mk_clear ~icty) clear in
  let hash = Option.map (mk_hash ~icty) hash in
  let compare = Option.map (mk_compare ~icty) compare in
  let init = Option.map (mk_init ~cty) init in
  let mem = Option.map (mk_mem ~cty) mem in
  custom ?init ?clear ?hash ?compare ?mem ~ml ~icty ~cty ()

let custom_ptr ?init ?clear ?hash ?compare ?mem ?malloc ~ml ~c () =
  let cty = expr "%s" c in
  let icty = cty in
  let clear = Option.map (mk_clear ~icty) clear in
  let hash = Option.map (mk_hash ~icty) hash in
  let compare = Option.map (mk_compare ~icty) compare in
  let init = Option.map (mk_init ~cty) init in
  let mem = Option.map (mk_mem ~cty) mem in
  custom_ptr ?init ?clear ?hash ?compare ?mem ?malloc ~ml ~cty ()

type value_type = { t : mlc; value : mlc; copy_of_ptr : mlc }

let value_type ?(init_args = []) ?hash ?compare ?mem ~init ~clear ~copy ~ml ~c
    () =
  let cty = expr "%s" c in
  let init_c = mk_init ~cty init ~vars:(fun v -> v :: init_args) in
  let clear_c = mk_init ~cty clear in
  let t =
    Expert.custom_ptr ~init:init_c ~clear:(mk_clear ~icty:cty clear)
      ?hash:(Option.map (mk_hash ~icty:cty) hash)
      ?compare:(Option.map (mk_compare ~icty:cty) compare)
      ?mem:(Option.map (mk_mem ~cty) mem)
      ~malloc:true ~ml ~cty ()
  in
  let value =
    let c = Var.mk "c" cty in
    let on_c (f : init) =
      Some (expr "%a" pp_expr_binds (f.init, [ (f.c, e_addr c) ]))
    in
    let value_c =
      {
        cty;
        init = on_c init_c;
        init_expr = expr "((%a) { 0 })" pp_expr cty;
        free = on_c clear_c;
        in_call = None;
        c;
      }
    in
    let mk ~src ~dst ~exprs = mk_converter ~src ~dst copy ~exprs in
    Expert.convert ~mlc:t ~c:value_c
      ~c_to_mlc:
        (mk ~src:value_c ~dst:t.cty ~exprs:(fun ~dst ~src ->
             [ e_deref dst; e_var src ]))
      ~mlc_to_c:
        (mk ~src:t.cty ~dst:value_c ~exprs:(fun ~dst ~src ->
             [ e_var dst; e_deref src ]))
      ()
  in
  let copy_of_ptr =
    Expert.copy t
      ~copy:
        (mk_copy ~cty:t.cty.cty copy ~exprs:(fun ~dst ~src ->
             [ e_deref dst; e_deref src ]))
  in
  { t; value; copy_of_ptr }

let extra_input ?label ?name ty =
  Expert.simple_param ?input_label:label ?name ~input:true ~used_in_call:false
    ty

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

let do_nothing ml = func_id ~ml ~call_params:[] ?result:None (expr "")

let convert ?c_to_mlc ?mlc_to_c ?(using = []) ~mlc ~c () =
  let mk ~src ~dst =
    Option.map (fun name ->
        Expert.mk_converter ~src ~dst name ~vars:(fun ~dst ~src ->
            [ dst; src ] @ using))
  in
  Expert.convert
    ?c_to_mlc:(mk c_to_mlc ~dst:mlc.cty ~src:c)
    ?mlc_to_c:(mk mlc_to_c ~dst:c ~src:mlc.cty)
    ~mlc ~c ()

let on_stack ?init_expr ?init ?clear cty =
  let cty = expr "%s" cty in
  let init = Option.map (mk_init ~cty) init in
  let clear = Option.map (mk_init ~cty) clear in
  let c = Var.mk "c" cty in
  let pp_f f =
    let pp_init fmt f =
      Fmt.pf fmt "%a" pp_expr_binds (f.init, [ (f.c, e_addr c) ])
    in
    expro "%a" Fmt.(option pp_init) f
  in
  {
    cty;
    init = pp_f init;
    init_expr =
      (match init_expr with
      | Some s -> expr "%s" s
      | None -> expr "((%a) { 0 })" pp_expr cty);
    free = pp_f clear;
    in_call = None;
    c;
  }

let int_trunc = c_int
let input_string = string_len
let output_string = string_len_ref
let input_array = array_len
let output_array = array_len_ref
