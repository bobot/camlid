open Expr
open Type

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

let results f =
  let out_params = List.filter (fun p -> p.output) f.params in
  match f.result with
  | None -> out_params
  | Some result ->
      if result.routput then
        {
          input = false;
          output = false;
          used_in_call = false;
          pty = result.rty;
          pc = result.rc;
          binds = result.binds;
        }
        :: out_params
      else out_params

let return_var = Var.mk "ret" (expr "value")

let code_c_fun (f : func) =
  let inputs =
    List.filter_map
      (fun p ->
        if p.input then Some (p, Var.mk p.pc.name (expr "value")) else None)
      f.params
  in
  let results = results f in
  let used_in_calls = List.filter (fun p -> p.used_in_call) f.params in
  let vars_used_in_calls = List.map (fun p -> p.pc) used_in_calls in
  (* C function declaration *)
  let fid =
    Helper.declare_existing
      ?result:(Option.map (fun r -> expr "%a" pp_code r.rty.cty) f.result)
      f.fname vars_used_in_calls
  in
  (* local C variable declaration *)
  let tuple_var = Var.mk "tup" (expr "value[%i]" (List.length results)) in
  let id = ID.mk ("stub_" ^ f.fname) in
  fp ~kind:C ~params:(List.map snd inputs) id (fun { fmt } ->
      (* Formals *)
      let pp_formal fmt (_, pv) = Fmt.pf fmt "value %a" pp_var pv in
      fmt "@[<hv>@[<hv 2>@[extern value %a@](%a)@[{@]@," pp_id id
        Fmt.(list ~sep:comma pp_formal)
        inputs;
      (* Locals *)
      let pp_local fmt p =
        Fmt.pf fmt "@[%a %a = %a;@]@," pp_code p.pty.cty pp_var p.pc init_expr
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_local) f.params;
      (match f.result with
      | None -> ()
      | Some result ->
          fmt "@[%a %a;@]@," pp_code result.rty.cty pp_var result.rc);
      fmt "@[value %a;@]@," pp_var return_var;
      (match results with
      | [] | [ _ ] -> ()
      | l ->
          fmt "@[value %a;@]@," pp_var return_var;
          fmt "@[value %a[%i] = {%a};@]@," pp_var tuple_var (List.length l)
            Fmt.(list ~sep:comma (Fmt.any "Val_unit"))
            l);
      (* convert input variables *)
      let pp_conv_in fmt ((p : param), vc) =
        Fmt.pf fmt "@[%a;@]@,"
          (ml2c ~binds:p.binds ~v:(expr "&%a" pp_var vc)
             ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_conv_in) inputs;
      (* initialize variables that are not input *)
      let pp_init_out fmt p =
        if not p.input then
          Fmt.pf fmt "@[%a;@]@,"
            (init ~binds:p.binds ~c:(expr "&%a" pp_var p.pc) ())
            p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_init_out) f.params;
      (* function call *)
      let pp_result fmt = function
        | None -> ()
        | Some r -> Fmt.pf fmt "%a = " pp_var r.rc
      in
      fmt "@[%a%a;@]@," pp_result f.result pp_call
        (fid, List.map (fun v -> (v, expr "%a" pp_var v)) vars_used_in_calls);
      (* create return value *)
      let pp_conv_out fmt (p : param) =
        Fmt.pf fmt "@[%a;@]@,"
          (c2ml ~binds:p.binds
             ~v:(expr "&%a" pp_var return_var)
             ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      (match results with
      | [] -> fmt "@[%a = Val_unit;@]@," pp_var return_var
      | [ p ] ->
          (* convert uniq output *)
          fmt "%a" pp_conv_out p
      | l ->
          let len = List.length l in
          let li = List.mapi (fun i p -> (i, p)) l in
          fmt "@[Begin_roots_block(%a, %i)]@," pp_var tuple_var len;
          (* convert outputs *)
          let pp_conv_out fmt (i, (p : param)) =
            Fmt.pf fmt "@[%a;@]@,"
              (c2ml
                 ~v:(expr "&%a[%i]" pp_var tuple_var i)
                 ~c:(expr "&%a" pp_var p.pc) ~binds:p.binds ())
              p.pty
          in
          fmt "%a" Fmt.(list ~sep:Fmt.cut pp_conv_out) li;
          (* create output tuple *)
          fmt "@[%a = caml_alloc(%i,0);]@," pp_var return_var len;
          let pp_store fmt (i, _) =
            Fmt.pf fmt "@[Store_field(%a, %i, %a);]@," pp_var return_var i
              pp_var tuple_var
          in
          fmt "%a" Fmt.(list ~sep:Fmt.cut pp_store) li;
          fmt "@[End_roots()]@,");
      (* free allocated memory *)
      let pp_init_out fmt (p : param) =
        Fmt.pf fmt "@[%a;@]@,"
          (free ~binds:p.binds ~c:(expr "&%a" pp_var p.pc) ())
          p.pty
      in
      fmt "%a" Fmt.(list ~sep:nop pp_init_out) f.params;
      (* return *)
      fmt "@[return %a;@]" pp_var return_var;
      fmt "@]@,@[};@]@]@.")

let print_c cout headers =
  output_string cout "#include <caml/mlvalues.h>\n";
  output_string cout "#include <caml/memory.h>\n";
  output_string cout "#include <caml/alloc.h>\n";
  output_string cout "#include <caml/custom.h>\n";
  List.iter
    (fun header -> Printf.fprintf cout "#include \"%s\"\n" header)
    headers

let print_ml_fun fmt f =
  let code_c = code_c_fun f in
  let results = results f in
  let inputs = List.filter (fun p -> p.input) f.params in
  let pp_result fmt p = pp_code fmt p.pty.mlty in
  let pp_param fmt p = Fmt.pf fmt "%a -> " pp_code p.pty.mlty in
  Fmt.pf fmt "@[external %s: %a%a = \"%a\"@]" f.fname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "*") pp_result)
    results pp_code code_c

let print_ml fmt l =
  (* Functions *)
  List.iter (function Fun f -> print_ml_fun fmt f) l

let to_file ?(prefix = "camlid") ?(headers = []) basename l =
  let cout_c = open_out (basename ^ "_stub.c") in
  print_c cout_c headers;
  let cout_ml = open_out (basename ^ ".ml") in
  let decl fmt = function Fun f -> print_ml_fun fmt f in
  final_print ~prefix ~ml:cout_ml ~c:cout_c ML
    (expr "%a" Fmt.(list ~sep:Fmt.cut decl) l);
  close_out cout_c;
  close_out cout_ml
