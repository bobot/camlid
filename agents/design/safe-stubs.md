# Proposal: a checkable stub IR, a cleaner C API, and Why3 proofs

Status (2026-10-21): points 1 to 3 are done. `Ty.contract` (ce37613),
the plan IR (9079aea), and `schedule` proved in Why3 (`proof/plan.mlw`,
extracted to `lib/plan_gen.ml`), with checks instead of raising
conversions. Points 4 (C API without `*%a`) and 5 (`Unchecked`) are not
started. The model and its limits are described in the header of
`proof/plan.mlw`.

Status (2026-10-23): `proof/spec.mlw` is the specification for reviewers.
It models the memory as blocks (OCaml blocks and C memory, one per
allocation site) with an owner, variables pointing to blocks, and the GC
as in Melocoton. The aliasing of the C function is a parameter of the spec
(`aliasing`). `schedule` (`proof/schedule.mlw`) is proved to satisfy
`safe_spec no_alias`; the per-parameter model of `plan.mlw` is removed.
The spec found that a `value` stored in a C array or struct is not
registered: such an input is now converted last, and two of them are
refused (`Unrooted_heap`). Next: decide how a template declares its
aliases (`keep_alive`, `with_parent`), and prove `schedule` for them.

Status (2026-10-26): to check that the block model is not wrong, two
generated stubs of a custom type (`to_int`, and `two` with two custom
outputs in a tuple) are proved in Melocoton (`proof/melocoton/`, Rocq,
`make melocoton`). The rules agree: values are kept in `CAMLlocal` roots
across allocations, and a custom block owns its C memory. Later, after a
discussion with the Melocoton authors: prove each kind of action of
`spec.mlw` sound in Melocoton, so that the spec is no longer trusted.

Status (2026-11-06): `spec.mlw` is too complicated to review, so a lower
level model is being tried: `lib/blocks.ml`, about 285 lines of OCaml for
the semantics, plus the translation from a plan. There are only blocks,
with a validity, an owner and the blocks they mention. A GC moves every
block owned by OCaml; a block mentioning a moved block becomes invalid,
except the roots and the blocks owned by OCaml, which the GC updates. C
memory owned by blocks dies when they are all dead. The model is
executable, and `Expert.plan` runs it on each stub (translation
validation). `proof/` is unchanged and still proves `schedule` against
`spec.mlw`. Next: write this model in Why3 so that it replaces `spec.mlw`,
and prove `schedule` against it. Still trusted: the translation in
`Blocks.of_plan`, and the contract of each template. Not done yet: templates
don't declare what an output keeps alive (`keeps`, for `keep_alive` and
`with_parent`), so the model only sees this in the tests of
`tests/blocks/`.

Todo: a stub with an `output_value` is never `noalloc`, which is safe and
simple. Later, the C function could say that it doesn't allocate, for example
when it only stores an existing value or an immediate.

## Goal

Camlid has three kinds of users:

- **Helper**: the safe default. Helper types come with a contract written by
  camlid, so in simple cases the user does the right thing without knowing
  the OCaml FFI rules.
- **Expert**: the user writes C templates in a formatter and declares their
  contract. Camlid checks how the stub uses them.
- **Hack**: anything goes, for prototypes. This stays possible, as in
  today's `expr "..."`.

Proofs should make the reviews easier. They should not slow down
prototyping. Proofs stay small: no proof of full behaviour, no C semantics.

## What is wrong today

1. `Expr.expr = { expr : unit Fmt.t }` is a closure. Its structure can only
   be observed by printing: dependencies are found with a dry run, and
   variables are substituted with `Format` stags. Nothing can be checked
   before printing.
2. Pointers are implicit. The functions created by `code`/`call_codef` take
   `value *v` and `T *c` and are called with `&v`, `&c`, so their bodies
   write `*%a` (84 times in `expert.ml`) and `->`. A missing `*` compiles
   when `T` is itself a pointer (`char **` vs `char *`), and can't be checked.
3. The stub is a fixed sequence of steps (ml2c, init, call, error, c2ml,
   free, return), but this sequence only exists as printing code in
   `Expert.code_c_fun` (180 lines). The facts that make it safe are scattered
   in booleans (`owned_by_ml`, `ml_heap`, `ml2c_noalloc`, `c2ml_noalloc`)
   and in `invalid_arg` checks in `param`, `code_c_fun` and `is_noalloc`.
4. `ml2c_noalloc` mixes up "doesn't allocate in the OCaml heap" and "doesn't
   raise". The second one matters for leaks. Known bug: if the ml2c of
   input 2 raises (`string_nt` with a null byte, a disposed value), the
   malloc'd copy of input 1 leaks.

## Proposal

### 1. An explicit contract per type (small, first)

Replace the four booleans of `Ty.mlc` with one record:

```ocaml
type effect = { ml_alloc : bool; raises : bool; c_alloc : bool }

type ownership =
  | Copied of { free : bool }   (* the stub owns the C copy *)
  | Owned_by_ml                 (* ml2c borrows, c2ml moves *)

type contract = {
  ownership : ownership;
  ml2c : effect;
  c2ml : effect;
  ml_heap : bool;   (* the C value points into the OCaml heap *)
}
```

Helper types fill it in. Expert constructors take it as an argument, with
the most pessimistic value as the default (allocates, raises, Copied with
free), so a user who doesn't know their contract gets a correct, slower stub.

### 2. A stub plan IR, separate from printing

`code_c_fun` becomes two steps:

```ocaml
type pid = int  (* index of a parameter *)

type step =
  | Ml2c of pid | Init of pid
  | Enter_blocking | Call | Leave_blocking
  | Check_error of step list   (* cleanup on the error path, then raise *)
  | C2ml of pid | Alloc_tuple | Store of pid
  | Free of pid | Return

type plan = {
  roots : pid list;     (* values registered with CAMLparam/CAMLlocal *)
  noalloc : bool;       (* ML external gets [@@noalloc] *)
  steps : step list;
}

val schedule : options -> contract list -> (plan, string) result
val print : plan -> params -> Expr.code   (* trusted, simple *)
```

`schedule` contains every decision: order, roots, noalloc, refusal of
`release_lock`, and the error path. `print` only translates each step into
the C text of the parameter.

The tests commit their generated `mylib_stub.c`, so this refactor is checked
by a zero diff on the promoted files.

### 3. Prove `schedule` in Why3, extract it

Write `schedule` in WhyML (`proof/stub_plan.mlw`) and extract it to
`lib/stub_plan_gen.ml`. Commit the extracted file, so camlid doesn't depend
on Why3 at build time. The extracted file must compile with OCaml 4.11.

The model is an abstract state, not C:

- each C variable is `Uninit | Owned | Borrowed | Moved | Freed`;
- each `value` variable is `rooted` or not, and `valid` or not;
- the runtime lock is held or released;
- a step has a precondition ("safe") and an effect. A step whose contract
  has `ml_alloc` invalidates the `value`s that are not rooted. A step that
  `raises` leaves the function.

The theorem says that `schedule` returns `Ok plan` exactly when the user
constraints hold, and then every execution of the plan (success, error path,
any raising step) satisfies these properties:

| | property | what it prevents |
|---|---|---|
| P1 | each Owned C value is freed or moved exactly once, on every path | leak, double free |
| P2 | the call and the conversions only read initialized, not freed, values | use before init, use after free |
| P3 | a `value` used after an allocating step is rooted; a `noalloc` plan has no allocating or raising step | GC bugs, wrong `[@@noalloc]` |
| P4 | between Enter and Leave blocking, no step reads the OCaml heap | data race with the GC |
| P5 | an Owned_by_ml C value is never moved into two OCaml values | double finalization |
| P6 | when a step raises, no Owned value is live (or the step is inside a cleanup) | leak on exceptions (the bug of point 4) |

The proof should find P6 on today's ordering. The fix in `schedule` is to
convert the raising inputs first, or check before converting.

Trusted: each C template matches its contract. The tests and valgrind
already check this, and the contracts are few.

Other small models, if they stay cheap:

- **parent/child reference count**: `count = 1 + live children`, and the
  parent is cleared exactly once, after its last child and its own
  finalizer.
- **dispose**: a disposed value is never cleared again (the NULL check),
  and ml2c refuses it.

Out of scope, kept as golden-file tests: printing, naming of variables,
deduplication, algdata macros.

### 4. A small C AST for the generated code, `raw` kept

This is independent of the proofs. It is for readability and to remove
`*%a`.

- **Typed variables**: `Var.mk name cty`, where `cty` is a small type
  (`Named of string | Ptr of cty | Value | Def of defined`). `Expr.deref v`
  is checked: it fails at generation time if `v` isn't a pointer.
- **By-reference formals**, as C++ `T&`: `Var.mk_ref "c" cty` prints
  `T *c` in the signature and `(*c)` in the body, and the caller passes
  `&arg`. Most `*%a` and `e_addr` disappear:

  ```ocaml
  (* today *)
  let v' = Var.mk "v" (expr "value *") and c' = Var.mk "c" (expr "%a *" pp_def cty) in
  call_codef "ml2c" [ (v', e_addr v); (c', e_addr c) ] (fun { fmt } ->
      fmt "*%a=malloc(len);@ " pp_var c'; ...)

  (* proposed: v and c are the template variables, passed by reference *)
  call_codef "ml2c" ~by_ref:[ v; c ] (fun { fmt } ->
      fmt "%a=malloc(len);@ " pp_var c; ...)
  ```

- **Lvalues and a few statements** (`C.( !* )`, `C.field`, `C.arrow`,
  `C.assign`, `C.if_`, `C.call`) for the code camlid writes itself. They
  print with the right parentheses. They are not required: `expr "..."`
  (= `C.raw`) stays, can be mixed with them, and keeps the variable
  substitution.
- Dependencies become data (the list of definitions used by an expression),
  computed when the expression is built. The dry run of `final_print` is
  kept only for `raw`.

### 5. Three API levels

- `Helper`: safe by construction, with contracts set by camlid.
- `Expert`: templates plus a declared contract. `schedule` refuses unsafe
  combinations with a clear message, as today, but from one place.
- `Expert.Unchecked` (or `~unchecked:true`): skips the refusal, for hacks
  and prototypes. The generated stub has a `/* camlid: unchecked */`
  comment.

## Order of work

Each step keeps `make test` green with a zero diff, or a reviewed diff, on
the generated stubs.

1. Contract record (1) and stub plan IR (2), in OCaml. A pure refactor.
2. Why3 model and proof of `schedule` (3), then replace the OCaml
   `schedule` by the extracted one. Fix P6.
3. By-reference formals and typed variables (4), then remove `*%a` file by
   file.
4. Refcount and dispose models, if step 2 was cheap.
5. `Unchecked` (5) and the documentation.

## Decisions (2026-10-21)

- The extracted code is committed.
- Failures don't raise where they happen. A conversion or check that can
  fail (inputs, outputs, `~error`, preconditions) sets a C error
  descriptor and jumps to a cleanup label. The descriptor is static and
  doesn't allocate (`{ kind; const char *msg }`, or a malloc'd message).
  The labels are in reverse order of construction, so each label frees
  exactly what was built before it, without flags. The OCaml exception is
  created and raised once, at the end, after the cleanup. Templates that
  still call `caml_failwith` are `raises = true` in their contract, and
  `schedule` places them where nothing is owned, or refuses them.
- Outputs follow the same rule. An output moved into an OCaml value is
  owned by the GC, so a later failure doesn't leak it. A copied output not
  converted yet is freed by its cleanup label.
- A failing step allocates when it raises, so it is never noalloc.
- Out of memory in the OCaml heap (`caml_alloc` raising `Out_of_memory`)
  is out of scope, as in other bindings.
- Besides `schedule` (proved to give safe plans), extract a proved
  `check : plan -> bool` (proved: `check p` implies `p` is safe). Expert
  users can then write their own plan and have it validated by proved code,
  without writing Why3. `Unchecked` stays for arbitrary C.
- Expressions stay `Format` text, but the holes (variables, identifiers,
  definitions, bindings) are recorded as data when the expression is built.
  Effects are inferred for the holes that are camlid code. Raw text has a
  declared effect (pessimistic by default), and a lint looks for
  `caml_alloc`, `caml_copy`, `caml_failwith`, `caml_raise` and `CAMLparam`
  in text declared as not allocating.
- Input checks go to OCaml as much as possible: a type or a function can
  carry an OCaml precondition (null byte in a string, equal lengths,
  bounds, the range of an enum, a zero divisor). It runs in the generated
  ML wrapper before the external, so it owns nothing when it raises, and
  the C conversion doesn't raise. These checks are hypotheses of the plan
  (trusted, simple OCaml).
- Errors detected after the call stay in C. On failure the outputs can't
  be converted, and returning a result ADT would allocate in the normal
  case of a function with one boxed result. The stub fills the error
  descriptor, runs the cleanup, and raises at the end.
- The ppx is not needed for the lint: the generator runs during the build,
  and only the final text has the computed names. A quotation ppx
  (`{%c| ... |}`) may come later as sugar that builds the same data.
- The minimum OCaml version is 4.14.
