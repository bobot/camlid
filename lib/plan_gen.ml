(* Extracted from proof/plan.mlw by make extract: do not edit *)
type effects = {
  alloc: bool;
  raising: bool;
  }

let is_pure (e: effects) : bool = not e.alloc && not e.raising

type input_kind =
  | No_input
  | Boxed_input
  | Unboxed_input

type output_kind =
  | No_output
  | Boxed_output
  | Unboxable_output

type param = {
  input: input_kind;
  output: output_kind;
  owned_by_ml: bool;
  init: bool;
  free: bool;
  value_locals: bool;
  ml_heap: bool;
  ml2c: effects;
  c2ml: effects;
  in_check: bool;
  out_check: bool;
  }

type options = {
  want_noalloc: (bool) option;
  release_lock: bool;
  error: bool;
  }

type result_kind =
  | Unit_result
  | One_value of (int)
  | One_unboxed of (int)
  | Tuple of ((int) list)

type gc_roots = {
  inputs: (int) list;
  outputs: (int) list;
  ret: bool;
  err: bool;
  c_values: (int) list;
  }

type cleanup = {
  moves: (int) list;
  frees: (int) list;
  }

type step =
  | Check_in of ((int) list)
  | Ml2c of ((int) list)
  | Init of ((int) list)
  | Call of ((int) list) * (bool)
  | Check_error of cleanup
  | Check_out of ((int) list) * cleanup
  | C2ml of ((int) list)
  | Alloc_tuple of ((int) list)
  | Free of ((int) list)
  | Return

type plan = {
  noalloc: bool;
  result: result_kind;
  roots: gc_roots;
  steps: step list;
  }

type sched_error =
  | Bad_call
  | Input_output_owned
  | Owned_unused
  | Raising_owning_input
  | Raising_output
  | Lock_ml_heap
  | Not_noalloc

type schedule_result =
  | Ok of plan
  | Error of sched_error

type sel =
  | S_boxed_input
  | S_input
  | S_output
  | S_init
  | S_free
  | S_value_locals
  | S_in_check
  | S_out_check
  | S_raising_input
  | S_quiet_input
  | S_move_error
  | S_move_out
  | S_input_output_owned
  | S_owned_unused
  | S_raising_owning_input
  | S_raising_output
  | S_not_noalloc

let has_input (q: param) : bool =
  match q.input with
  | No_input -> false
  | _ -> true

let boxed_input (q: param) : bool =
  match q.input with
  | Boxed_input -> true
  | _ -> false

let has_output (q: param) : bool =
  match q.output with
  | No_output -> false
  | _ -> true

let unboxable_output (q: param) : bool =
  match q.output with
  | Unboxable_output -> true
  | _ -> false

let selected (x: sel) (q: param) : bool =
  match x with
  | S_boxed_input -> boxed_input q
  | S_input -> has_input q
  | S_output -> has_output q
  | S_init -> not (has_input q) && q.init
  | S_free -> q.free
  | S_value_locals -> q.value_locals
  | S_in_check -> has_input q && q.in_check
  | S_out_check -> has_output q && q.out_check
  | S_raising_input -> has_input q && q.ml2c.raising
  | S_quiet_input -> has_input q && not q.ml2c.raising
  | S_move_error ->
    has_output q && q.owned_by_ml && not (has_input q) && q.init
  | S_move_out -> has_output q && q.owned_by_ml
  | S_input_output_owned -> has_input q && has_output q && q.owned_by_ml
  | S_owned_unused ->
    not (has_input q) && not (has_output q) && q.owned_by_ml
  | S_raising_owning_input -> has_input q && q.ml2c.raising && q.free
  | S_raising_output -> has_output q && q.c2ml.raising
  | S_not_noalloc ->
    has_input q && (not (is_pure q.ml2c) || q.in_check) || has_output q && 
                                                           q.out_check

let rec pids (x: sel) (ps: param list) (n: int) : (int) list =
  match ps with
  | [] -> [] 
  | q :: r ->
    (let l = pids x r ((n + 1)) in if selected x q then n :: l else l)

let rec exists_sel (x: sel) (ps: param list) : bool =
  match ps with
  | [] -> false
  | q :: r -> (let b = exists_sel x r in selected x q || b)

let rec get (ps: param list) (i: int) : param =
  match ps with
  | [] -> assert false (* absurd *)
  | q :: r -> if ((i = 0)) then q else get r ((i - 1))

let rec valid_call (n: int) (l: (int) list) : bool =
  match l with
  | [] -> true
  | i :: r -> ((0 <= i)) && ((i < n)) && valid_call n r

let rec heap_in_call (ps: param list) (l: (int) list) : bool =
  match l with
  | [] -> false
  | i :: r ->
    (let q = get ps i in q.ml_heap || q.value_locals || heap_in_call ps r)

let schedule (o: options) (ps: param list) (call: (int) list) :
  schedule_result =
  if not (valid_call (List.length ps) call)
  then Error Bad_call
  else
    begin
      if exists_sel S_input_output_owned ps
      then Error Input_output_owned
      else
        begin
          if exists_sel S_owned_unused ps
          then Error Owned_unused
          else
            begin
              if exists_sel S_raising_owning_input ps
              then Error Raising_owning_input
              else
                begin
                  if exists_sel S_raising_output ps
                  then Error Raising_output
                  else
                    begin
                      if o.release_lock && heap_in_call ps call
                      then Error Lock_ml_heap
                      else
                        begin
                          let outputs1 = pids S_output ps 0 in
                          let can_noalloc =
                            not o.release_lock && not o.error && not 
                                                                 (exists_sel S_not_noalloc
                                                                  ps) && 
                                                                 begin
                                                                   match
                                                                   outputs1
                                                                 with
                                                                 | [] -> true
                                                                 | i :: ([]) ->
                                                                   (let q =
                                                                    get ps i in
                                                                    is_pure 
                                                                    q.c2ml || 
                                                                    unboxable_output q)
                                                                 | _ -> false
                                                                 end in
                          let noalloc1 =
                            match o.want_noalloc with
                            | None -> can_noalloc
                            | Some b -> b in
                          if noalloc1 && not can_noalloc
                          then Error Not_noalloc
                          else
                            begin
                              let result1 =
                                match outputs1 with
                                | [] -> Unit_result
                                | i :: ([]) ->
                                  if unboxable_output (get ps i)
                                  then One_unboxed i
                                  else One_value i
                                | l -> Tuple l in
                              let out_checks = pids S_out_check ps 0 in
                              let roots1 =
                                if noalloc1
                                then
                                  { inputs = [] ; outputs = [] ; ret = false;
                                    err = false; c_values = []  }
                                else
                                  { inputs = pids S_boxed_input ps 0;
                                    outputs =
                                    begin match result1 with
                                    | (Unit_result | One_unboxed _) -> [] 
                                    | One_value i -> i :: [] 
                                    | Tuple l -> l
                                    end; ret =
                                    begin match result1 with
                                    | Tuple _ -> true
                                    | _ -> false
                                    end; err =
                                    o.error || begin match out_checks with
                                    | [] -> false
                                    | _ -> true
                                    end; c_values =
                                    pids S_value_locals ps 0 } in
                              let cin = pids S_in_check ps 0 in
                              let rin = pids S_raising_input ps 0 in
                              let qin = pids S_quiet_input ps 0 in
                              let ini = pids S_init ps 0 in
                              let frees1 = pids S_free ps 0 in
                              let cerr =
                                { moves = pids S_move_error ps 0; frees =
                                  frees1 } in
                              let cout =
                                { moves = pids S_move_out ps 0; frees =
                                  frees1 } in
                              let t_free = Free frees1 :: Return :: []  in
                              let t_tuple =
                                match result1 with
                                | Tuple l -> Alloc_tuple l :: t_free
                                | _ -> t_free in
                              let t_c2ml = C2ml outputs1 :: t_tuple in
                              let t_out =
                                Check_out (out_checks, cout) :: t_c2ml in
                              let t_err =
                                if o.error
                                then Check_error cerr :: t_out
                                else t_out in
                              let t_call =
                                Call (call, o.release_lock) :: t_err in
                              let t_init = Init ini :: t_call in
                              let t_qin = Ml2c qin :: t_init in
                              let t_rin = Ml2c rin :: t_qin in
                              let steps1 = Check_in cin :: t_rin in
                              let p =
                                { noalloc = noalloc1; result = result1;
                                  roots = roots1; steps = steps1 } in
                              Ok p end end end end end end end

