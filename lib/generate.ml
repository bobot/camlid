open Type

let list_or_empty ~empty ~sep pp fmt = function
  | [] -> empty fmt ()
  | l -> Fmt.list ~sep pp fmt l

let collect l =
  let h = Hashtbl.create 10 in
  let q = Queue.create () in
  let collect_decl = function
    | Fun { params; result; fname = _ } ->
        let rec collect_typedef (td : typedef) =
          match Hashtbl.find_opt h td.name with
          | None ->
              Hashtbl.add h td.name td;
              List.iter collect_typedef td.deps;
              Queue.add td q
          | Some td' ->
              if not (td == td') then
                Fmt.failwith
                  "Two different typedefs have the same name %s: %s and %s"
                  td.name td.descr td'.descr
        in
        Option.iter (fun { rty; routput = _ } -> collect_typedef rty) result;
        List.iter (fun param -> collect_typedef param.pty) params
  in
  List.iter collect_decl l;
  List.of_seq @@ Queue.to_seq q

(* Name of the variable for the result of the C function *)
let result_name = "_res"

(* Name of the variable for the return Stub function *)
let return_name = "_ret"

(* Name of the variable for the intermediary tuple construction *)
let tuple_name = "_tup"

let results f =
  let out_params = List.filter (fun p -> p.output) f.params in
  match f.result with
  | None -> out_params
  | Some result ->
      if result.routput then
        { input = false; output = false; pty = result.rty; pname = result_name }
        :: out_params
      else out_params

let print_c_fun fmt f =
  let inputs = List.filter (fun p -> p.input) f.params in
  let results = results f in
  (* C function declaration *)
  let pp_result fmt = function
    | None -> Fmt.string fmt "void"
    | Some { rty; _ } -> cty fmt rty
  in
  let pp_param fmt p = cty fmt p.pty in
  Fmt.pf fmt "@[<v 2>@[%a %s@](%a)@];@." pp_result f.result f.fname
    Fmt.(list ~sep:comma pp_param)
    f.params;
  (* Stub function formals *)
  let c_name fmt s = Fmt.pf fmt "c_%s" s in
  let v_name fmt s = Fmt.pf fmt "v_%s" s in
  let pp_param fmt p = Fmt.pf fmt "value %a" v_name p.pname in
  Fmt.pf fmt "@[<v 2>@[extern value %a@](%a)@[{@]@," stub_name f
    Fmt.(list ~sep:comma pp_param)
    inputs;
  (* local C variable declaration *)
  let pp_local fmt p =
    Fmt.pf fmt "@[%a %a = %a;@]@," cty p.pty c_name p.pname p.pty.init_expr ()
  in
  Fmt.(list ~sep:nop pp_local) fmt f.params;
  (match f.result with
  | None -> ()
  | Some result -> Fmt.pf fmt "@[%a %a;@]@," cty result.rty c_name result_name);
  (match results with
  | [] -> ()
  | [ _ ] -> Fmt.pf fmt "@[value %a;@]@," v_name return_name
  | l ->
      Fmt.pf fmt "@[value %a;@]@," v_name return_name;
      Fmt.pf fmt "@[value %a[%i] = {%a};@]@," v_name tuple_name (List.length l)
        Fmt.(list ~sep:comma (Fmt.any "Val_unit"))
        l);
  (* convert input variables *)
  let pp_conv_in fmt p =
    if p.input then
      Fmt.pf fmt "@[%a(&%a,&%a);@]@," ml2c p.pty c_name p.pname v_name p.pname
  in
  Fmt.(list ~sep:nop pp_conv_in) fmt f.params;
  (* initialize output variables thatn are not input *)
  let pp_init_out fmt p =
    if (not p.input) && p.output then
      Fmt.pf fmt "@[%a(&%a);@]@," init p.pty c_name p.pname
  in
  Fmt.(list ~sep:nop pp_init_out) fmt f.params;
  (* function call *)
  let pp_arg fmt p = Fmt.pf fmt "%a" c_name p.pname in
  let pp_result fmt = function
    | None -> ()
    | Some _ -> Fmt.pf fmt "%a = " c_name result_name
  in
  Fmt.pf fmt "@[%a%s(%a);@]@," pp_result f.result f.fname
    Fmt.(list ~sep:comma pp_arg)
    f.params;
  (* create return value *)
  (match results with
  | [] -> Fmt.pf fmt "@[return Val_unit;@]@,"
  | [ p ] ->
      (* convert uniq output *)
      Fmt.pf fmt "@[%a(&%a,&%a);@]@," c2ml p.pty v_name return_name c_name
        p.pname;
      Fmt.pf fmt "@[return %a;@]@," v_name return_name
  | l ->
      let len = List.length l in
      let li = List.mapi (fun i p -> (i, p)) l in
      Fmt.pf fmt "@[Begin_roots_block(%a, %i)]@," v_name tuple_name len;
      (* convert outputs *)
      let pp_conv_out fmt (i, p) =
        Fmt.pf fmt "@[%a(&%a[%i],&%a);]@," c2ml p.pty v_name tuple_name i c_name
          p.pname
      in
      Fmt.(list ~sep:Fmt.cut pp_conv_out) fmt li;
      (* create output tuple *)
      Fmt.pf fmt "@[%a = caml_alloc(%i,0);]@," v_name return_name len;
      let pp_store fmt (i, _) =
        Fmt.pf fmt "@[Store_field(%a, %i, %a);]@," v_name return_name i v_name
          tuple_name
      in
      Fmt.(list ~sep:Fmt.cut pp_store) fmt li;
      Fmt.pf fmt "@[End_roots()]@,";
      Fmt.pf fmt "@[return %a;@]@," v_name return_name);
  Fmt.pf fmt "@]};@."

let print_c fmt headers l =
  let tds = collect l in
  Fmt.pf fmt "#include <caml/mlvalues.h>@.";
  Fmt.pf fmt "#include <caml/memory.h>@.";
  Fmt.pf fmt "#include <caml/alloc.h>@.";
  Fmt.pf fmt "#include <caml/custom.h>@.";
  let pp_header fmt header = Fmt.pf fmt "#include \"%s\"@." header in
  Fmt.(list ~sep:(any "@.") pp_header) fmt headers;
  (* Forward declarations *)
  List.iter
    (fun td ->
      Fmt.pf fmt "/* %s: %s */@." td.name td.descr;
      Fmt.pf fmt "typedef %a %a;@." td.cty () cty td;
      Fmt.pf fmt "static void %a(value *, %a *);@." c2ml td cty td;
      Fmt.pf fmt "static void %a(%a *, value *);@." ml2c td cty td;
      Fmt.pf fmt "static void %a(%a *);@." init td cty td;
      Fmt.pf fmt "@.")
    tds;
  (* Definitions *)
  List.iter
    (fun td ->
      Fmt.pf fmt "/* %s: %s */@." td.name td.descr;
      td.extra_defs fmt ();
      Fmt.pf fmt "static void %a%a;@." c2ml td td.c2ml ();
      Fmt.pf fmt "static void %a%a;@." ml2c td td.ml2c ();
      Fmt.pf fmt "static void %a%a;@." init td td.init ();
      Fmt.pf fmt "@.")
    tds;
  (* Functions *)
  List.iter (function Fun f -> print_c_fun fmt f) l

let print_ml_fun fmt f =
  let results = results f in
  let inputs = List.filter (fun p -> p.input) f.params in
  let pp_result fmt p = mlty fmt p.pty in
  let pp_param fmt p = Fmt.pf fmt "%a -> " mlty p.pty in
  Fmt.pf fmt "@[external %s: %a%a = \"%a\"@]" f.fname
    Fmt.(list_or_empty ~empty:(any "unit -> ") ~sep:nop pp_param)
    inputs
    Fmt.(list_or_empty ~empty:(any "unit") ~sep:(any "*") pp_result)
    results stub_name f

let print_ml fmt l =
  let tds = collect l in
  (* type def *)
  List.iter
    (fun td ->
      Fmt.pf fmt "(** %s: %s *)@." td.name td.descr;
      Fmt.pf fmt "type %a%a@." mlty td td.mlty ();
      Fmt.pf fmt "@.")
    tds;
  (* Functions *)
  List.iter (function Fun f -> print_ml_fun fmt f) l

let to_file ?(headers = []) basename l =
  let cout = open_out (basename ^ "_stub.c") in
  let fmt = Format.formatter_of_out_channel cout in
  print_c fmt headers l;
  Fmt.flush fmt ();
  close_out cout;
  let cout = open_out (basename ^ ".ml") in
  let fmt = Format.formatter_of_out_channel cout in
  print_ml fmt l;
  Fmt.flush fmt ();
  close_out cout
