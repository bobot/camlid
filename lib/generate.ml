open Type

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
        Option.iter collect_typedef result;
        List.iter (fun param -> collect_typedef param.pty) params
  in
  List.iter collect_decl l;
  List.of_seq @@ Queue.to_seq q

let print_c fmt l =
  let tds = collect l in
  Fmt.pf fmt "#include <caml/mlvalues.h>@.";
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
      Fmt.pf fmt "static void %a%a;@." c2ml td td.c2ml ();
      Fmt.pf fmt "static void %a%a;@." ml2c td td.ml2c ();
      Fmt.pf fmt "static void %a%a;@." init td td.init ();
      Fmt.pf fmt "@.")
    tds;
  (* Functions *)
  List.iter
    (function
      | Fun f ->
          let pp_result fmt = function
            | None -> Fmt.string fmt "void"
            | Some td -> mlty fmt td
          in
          let pp_param fmt _ = Fmt.pf fmt "value" in
          Fmt.pf fmt "@[<v 2>@[%a %s@](%a)@];@." pp_result f.result f.fname
            Fmt.(list ~sep:comma pp_param)
            f.params;
          let c_name fmt s = Fmt.pf fmt "c_%s" s in
          let v_name fmt s = Fmt.pf fmt "v_%s" s in
          let pp_result fmt = function
            | None -> Fmt.string fmt "void"
            | Some td -> cty fmt td
          in
          let pp_param fmt p = Fmt.pf fmt "value %a" v_name p.pname in
          Fmt.pf fmt "@[<v 2>@[extern %a camlid_fun_%s@](%a)@[{@]@," pp_result
            f.result f.fname
            Fmt.(list ~sep:comma pp_param)
            f.params;
          let pp_local fmt p =
            Fmt.pf fmt "@[%a %a%a;@]@," cty p.pty c_name p.pname p.pty.init_expr
              ()
          in
          Fmt.(list ~sep:nop pp_local) fmt f.params;
          let pp_conv_in fmt p =
            Fmt.pf fmt "@[%a(&%a,&%a);@]@," ml2c p.pty c_name p.pname v_name
              p.pname
          in
          Fmt.(list ~sep:nop pp_conv_in) fmt f.params;
          let pp_arg fmt p = Fmt.pf fmt "%a" c_name p.pname in
          Fmt.pf fmt "@[%s(%a);@]@," f.fname
            Fmt.(list ~sep:comma pp_arg)
            f.params;
          Fmt.pf fmt "@]};@.")
    l

let print_ml fmt l =
  let tds = collect l in
  (* type def *)
  List.iter
    (fun td ->
      Fmt.pf fmt "(** %s: %s *)@." td.name td.descr;
      Fmt.pf fmt "type %a = %a@." mlty td td.mlty ();
      Fmt.pf fmt "@.")
    tds;
  (* Functions *)
  List.iter
    (function
      | Fun f ->
          let pp_result fmt = function
            | None -> Fmt.string fmt "unit"
            | Some td -> mlty fmt td
          in
          let pp_param fmt p = Fmt.pf fmt "%a -> " mlty p.pty in
          Fmt.pf fmt "@[external %s: %a%a = \"camlid_fun_%s\"@]" f.fname
            Fmt.(list ~sep:nop pp_param)
            f.params pp_result f.result f.fname)
    l

let to_file basename l =
  let cout = open_out (basename ^ "_stub.c") in
  let fmt = Format.formatter_of_out_channel cout in
  print_c fmt l;
  Fmt.flush fmt ();
  close_out cout;
  let cout = open_out (basename ^ ".ml") in
  let fmt = Format.formatter_of_out_channel cout in
  print_ml fmt l;
  Fmt.flush fmt ();
  close_out cout
