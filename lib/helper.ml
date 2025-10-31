open Expr
open Type

let typedef name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> dfp id "typedef %t %a;@." k pp_id id)

let mlalias name =
  let id = ID.mk name in
  Format.kdprintf (fun k -> dfp ~kind:ML id "type %a = %t@." pp_id id k)

let mlabstract name =
  let id = ID.mk name in
  dfp ~kind:ML id "type %a@." pp_id id

let declare_existing ?(result = expr "void") f params =
  let id = ID.mk ~keep_name:true f in
  let pp_ty fmt (var : var) = var.ty.expr fmt () in
  dfp ~params id "@[<hv 2>@[%a %s(@]%a@[);@]@]@." result.expr () f
    Fmt.(list ~sep:(any ",@ ") pp_ty)
    params

(** Native integer, the last bit is lost during translation *)
let int : typedef =
  let cty = typedef "int" "int" in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "int";
    cty;
    mlty = mlalias "int" "int";
    mlname = None;
    c2ml = code "c2ml" "*%a = Val_int(*%a);" pp_var v pp_var c;
    ml2c = code "ml2c" "*%a = Int_val(*%a);" pp_var c pp_var v;
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "0";
    v;
    c;
  }

let ptr_ref (ty : typedef) =
  let cty = typedef "ref" "%a *" pp_code ty.cty in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
  {
    descr = "ref on " ^ ty.descr;
    cty;
    mlty = mlalias "ptr_ref" "%a" pp_code ty.mlty;
    mlname = None;
    c2ml =
      code "c2ml" "%a;"
        (c2ml ~v:(expr "%a" pp_var v) ~c:(expr "*%a" pp_var c))
        ty;
    ml2c =
      code "ml2c" "%a;"
        (ml2c ~v:(expr "%a" pp_var v) ~c:(expr "*%a" pp_var c))
        ty;
    init = code "init" "%a;" (init ~c:(expr "*%a" pp_var c)) ty;
    init_expr =
      expr "&(((struct { %a a; }) { %a }).a)" pp_code ty.cty init_expr ty;
    v;
    c;
  }

type get = {
  get : code;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

type set = {
  set : code;
  c : var;  (** external type*)
  i : var;  (** internal type *)
}

(** Encapsulate a c type into an abstract ml type *)
let abstract' ?get ?set ~icty ~descr ~ml ~cty () =
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code (cty : code)) in
  {
    descr;
    cty;
    v;
    c;
    mlty = mlabstract ml;
    mlname = Some ml;
    ml2c =
      (match get with
      | None ->
          code "ml2c" "*%a = *((%a *) Bp_val(*%a));" pp_var c pp_code icty
            pp_var v
      | Some f ->
          code "ml2c" "%a;" pp_call
            ( f.get,
              [
                (f.c, expr "%a" pp_var c);
                (f.i, expr "((%a *) Bp_val(*%a))" pp_code icty pp_var v);
              ] ));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt
            "@[*%a = caml_alloc((sizeof(%a) + sizeof(value) - 1) / \
             sizeof(value), Abstract_tag);@]@,"
            pp_var v pp_code icty;
          match set with
          | None ->
              fmt "*((%a *) Bp_val(*%a)) = *%a;" pp_code icty pp_var v pp_var c
          | Some f ->
              fmt "%a;" pp_call
                ( f.set,
                  [
                    (f.c, expr "%a" pp_var c);
                    (f.i, expr "((%a *) Bp_val(*%a))" pp_code icty pp_var v);
                  ] ));
    init = code ~ovars:[ c ] "init" "";
    init_expr = expr "((%a) { })" pp_code cty;
  }

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
  abstract' ?set ?get ~icty ~descr ~cty ~ml ()

type finalize = { finalize : code; i : var }
type finalize_op = { finalize_op : code; v : var }
type initialize = { initialize : code; c : var }
type hash = { hash : code; i : var }
type hash_op = { hash_op : code; v : var }
type compare = { compare : code; i1 : var; i2 : var }
type compare_op = { compare_op : code; v1 : var; v2 : var }

(** Encapsulate a c type into an custom ml type *)
let custom ?finalize ?initialize ?hash ?compare ?get ?set ?internal ~ml ~c () =
  let descr = Printf.sprintf "abstract tag for type \"%s\"" c in
  let cty = typedef "custom" "%s" c in
  let icty =
    match internal with None -> cty | Some c -> typedef "custom_intern" "%s" c
  in
  let v = Var.mk "v" (expr "value *") in
  let c = Var.mk "c" (expr "%a *" pp_code cty) in
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
  let finalize =
    Option.map
      (fun finalize ->
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { finalize = declare_existing finalize [ i ]; i })
      finalize
  in
  let data_custom_val icty v =
    expr "(%a *) Data_custom_val(%a)" pp_code icty pp_var v
  in
  let data_custom_val' icty v =
    expr "(%a *) Data_custom_val(*%a)" pp_code icty pp_var v
  in
  let finalize_op =
    Option.map
      (fun finalize ->
        let v = Var.mk "v" (expr "value") in
        {
          finalize_op =
            code "finalize_op" "%a;" pp_call
              (finalize.finalize, [ (finalize.i, data_custom_val icty v) ]);
          v;
        })
      finalize
  in
  let hash =
    Option.map
      (fun hash ->
        let i = Var.mk "i" (expr "%a *" pp_code icty) in
        { hash = declare_existing ~result:(expr "intnat") hash [ i ]; i })
      hash
  in
  let hash_op =
    Option.map
      (fun hash ->
        let v = Var.mk "v" (expr "value") in
        {
          hash_op =
            code ~ret:"intnat" "hash_op" "return %a;" pp_call
              (hash.hash, [ (hash.i, data_custom_val icty v) ]);
          v;
        })
      hash
  in
  let compare =
    Option.map
      (fun compare ->
        let i1 = Var.mk "c" (expr "%a *" pp_code icty) in
        let i2 = Var.mk "i" (expr "%a *" pp_code icty) in
        {
          compare = declare_existing ~result:(expr "int") compare [ i1; i2 ];
          i1;
          i2;
        })
      compare
  in
  let compare_op =
    Option.map
      (fun compare ->
        let v1 = Var.mk "v1" (expr "value") in
        let v2 = Var.mk "v2" (expr "value") in
        {
          compare_op =
            code ~ret:"int" "compare_op" "return %a;" pp_call
              ( compare.compare,
                [
                  (compare.i1, data_custom_val icty v1);
                  (compare.i2, data_custom_val icty v2);
                ] );
          v1;
          v2;
        })
      compare
  in
  let initialize =
    Option.map
      (fun finalize ->
        let c = Var.mk "c" (expr "%a *" pp_code cty) in
        { initialize = declare_existing finalize [ c ]; c })
      initialize
  in
  let custom_op =
    let id = ID.mk "cops" in
    let pp_op op_name get fmt = function
      | None -> Fmt.pf fmt "custom_%s_default" op_name
      | Some f -> Fmt.pf fmt "%a" pp_code (get f)
    in
    toplevel id
      "struct custom_operations %a = {\n\
       NULL,\n\
       %a,\n\
       %a,\n\
       %a,\n\
       custom_serialize_default,\n\
       custom_deserialize_default\n\
       };@."
      pp_id id
      (pp_op "finalize" (fun f -> f.finalize_op))
      finalize_op
      (pp_op "compare" (fun f -> f.compare_op))
      compare_op
      (pp_op "hash" (fun f -> f.hash_op))
      hash_op
  in
  {
    descr;
    cty;
    mlty = mlabstract ml;
    mlname = Some ml;
    ml2c =
      (match get with
      | None ->
          code "ml2c" "*%a = *(%a);" pp_var c (data_custom_val' icty v).expr ()
      | Some f ->
          code "ml2c" "@[%a;@]" pp_call
            ( f.get,
              [ (f.i, data_custom_val' icty v); (f.c, expr "%a" pp_var c) ] ));
    c2ml =
      codef "c2ml" (fun { fmt } ->
          fmt "@[*%a = caml_alloc_custom(&%a,sizeof(%a), 0, 1);@]@," pp_var v
            pp_code custom_op pp_code icty;
          match set with
          | None ->
              fmt "@[*(%a) = *%a;@]" (data_custom_val' icty v).expr () pp_var c
          | Some f ->
              fmt "@[%a;@]" pp_call
                ( f.set,
                  [ (f.i, data_custom_val' icty v); (f.c, expr "%a" pp_var c) ]
                ));
    init =
      (let pp_init fmt f =
         Fmt.pf fmt "%a;" pp_call (f.initialize, [ (f.c, expr "%a" pp_var c) ])
       in
       code ~ovars:[ c ] "init" "%a" Fmt.(option pp_init) initialize);
    init_expr = expr "((%a) { })" pp_code cty;
    v;
    c;
  }

let simple_param ?(input = false) ?(output = false) pty pname =
  {
    input;
    output;
    used_in_call = true;
    pty;
    pc = Var.mk pname (expr "%a" pp_code pty.cty);
  }

let input = simple_param ~input:true
let output = simple_param ~output:true
let inout = simple_param ~input:true ~output:true
let ignored = simple_param ~input:false ~output:false

let func fname ?result ?ignored_result params =
  match (result, ignored_result) with
  | Some _, Some _ ->
      failwith "Camlid.Helper.func: can't set both result and ignored_result"
  | Some rty, None ->
      Fun
        {
          fname;
          params;
          result =
            Some
              {
                rty;
                routput = true;
                rc = Var.mk "res" (expr "%a" pp_code rty.cty);
              };
        }
  | None, Some rty ->
      Fun
        {
          fname;
          params;
          result =
            Some
              {
                rty;
                routput = false;
                rc = Var.mk "res" (expr "%a" pp_code rty.cty);
              };
        }
  | None, None -> Fun { fname; params; result = None }
