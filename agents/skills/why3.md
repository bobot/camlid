<!-- Copied from colibrics (git.frama-c.com/pub/colibrics, branch
bobot/congruence_proved, 9ccb47a, agents/skills/why3.md) on 2026-10-21.
Keep the generic parts in sync with it. The section "In camlid" is local. -->

## In camlid

- Proofs go in `proof/*.mlw`, with the hints in `proof/why3find.proofs/`.
  The extracted OCaml (`lib/*_gen.ml`) is committed, so building camlid
  doesn't need Why3. It must compile with OCaml 4.11.
- Prove what `agents/design/safe-stubs.md` lists (memory safety of the stub
  plan), not the full behaviour. The C templates are trusted and tested.
- Environment (2026-10-21): why3 1.8.2 and why3find 1.3.0 are installed, and
  so are the provers `alt-ergo` and `colibri2`. `~/.why3.conf` is missing:
  run `why3 config detect` and `why3find config --detect`, and set
  `memlimit = 2000`. Get z3 and cvc5 with `why3find download`.
- The user is the modelling expert: report a wrong model or spec with a
  counterexample, and wait.

# Why3 & why3find Skills Reference

Generic guidance for proving with Why3/WhyML and `why3find`. Project-specific
lessons (e.g. the `bddbitv` formalization) live in `bddbitv.md` (colibrics only) — load that only
when working on that codebase.

IMPORTANT: The user is an expert in Why3 and handles modelling problems. You MUST
write the proof given the current modelling; when there is a problem, immediately
ask the user rather than redesigning the model. A *false* axiom or spec (find a
small numeric counterexample) is such a problem: report it with the
counterexample and a proposed fix, then wait.

---

## Skill 1: why3find workflow

### 1. Project initialization

Ensure `why3find.json` exists in the project root; if missing, run
`why3find config --detect` (auto-detects installed SMT solvers). Provers,
timeout and tactics are configured there (`-p z3 -p cvc5 -p alt-ergo`, `-t <sec>`).

### 2. Running proofs

```bash
why3find prove -j 4 [files...]              # standard run, at most 4 provers in parallel
why3find prove -x [files...]                # + source locations of unproved goals
why3find prove -X [files...]                # + generated VC of unproved goals
why3find prove -g <Theory.goal> [files...]  # focus on one goal (repeatable: -g A -g B)
why3find prove -a [files...]                # report remaining axioms / parameters
```

Goal names for `-g`:
- plain module: `Mod.fn` (e.g. `IBDD.mk`).
- refinement obligations of `module M : I`: `M'impl.<name>` (e.g.
  `Zbit'impl.bittest_sub_lsl`). Interface axioms checked against the concrete
  module appear as `M'impl.M'impl_of.<axiom>` in the output. **Always also run the
  `'impl` goal**: a lemma proved in `M` can still fail in `M'impl` (different
  context).

### 3. Load, cache, and how to iterate

- A full run launches many provers in parallel (≈8 CPU-seconds per wall-second).
  Borderline goals time out under load. **Always pass `-j 4`**, iterate with
  focused `-g` runs, run the whole file only at checkpoints, and never run two
  `why3find` processes at the same time.
- why3find **caches** results in `why3find.proofs/<file>.json`. Re-running an
  unchanged goal replays the cache and changes nothing: to make progress you must
  change the proof (add an assert, call a lemma, split).
- The cache stores *which* prover succeeded. A goal proved by an unusual prover
  (e.g. `spass` in ~5 s) on a goal that looks false is a red flag for an
  inconsistent context (see §5).

### 4. Iterative verification workflow

1. **Locate unproven goals** (`-x`), then read the VC (`-X -g <goal>`): the
   `goal ... :` line tells which assert / precondition / postcondition failed.
2. **Decompose**: insert intermediate `assert { ... }` right before the failing
   VC; split complex postconditions into several smaller `ensures`.
3. **Loops**: invariants valid on entry, preserved, strong enough; every loop and
   recursion has a decreasing `variant`.
4. **Re-run** the focused goal after each change; keep the file green.

### 5. Inconsistency: false axioms "prove" everything

An axiom of an interface is an *assumption* for every later goal of the
`'impl` theory and for every client. A single false axiom (e.g. `=` where `<=`
is meant) makes many unrelated goals pass. Consequences:
- Sanity-check every new axiom with a tiny counterexample search before proving.
- After fixing a false axiom, expect **downstream goals to start failing**: they
  were only proved through the inconsistency. This is progress, not regression.
- `why3find prove -a` lists the remaining axioms/hypotheses — check that only
  intended abstract ones remain.

### 6. Grepping the output

| What | Marker / example |
| :--- | :--- |
| Theory not fully proved | `Theory <file>.<Mod>: ✘ (n/m)` |
| Unproved goal | `Goal <Mod>.<fn>: ✘ (n/m)` |
| Source location of failure | `File "<file>.mlw", line N, characters a-b: proof failed` |
| VC location + kind (with `-X`) | `[<file>.mlw:N] <fn> [postcondition]` (`precondition`, `assertion`, `loop invariant preservation`, `variant decrease`, ...) |
| Generated VC text (with `-X`) | `goal <fn>'vc : <formula>` |
| Overall unproved count | `Error: N unproved file(s)` |
| Parse / type errors | `Fatal error: exception ...` (`syntax error`, `unbound function or predicate symbol`, `This term has type int, but is expected to have type ...`) |
| All green | `Proved (<file>.mlw) ✔` and every `Theory ...: ✔` |

Useful filters:
`... 2>&1 | grep "✘\|failed\|Fatal\|rror"` and, for the VC,
`why3find prove -j 4 -X -g G f.mlw 2>&1 | grep -A6 "^goal "`.
`Warning ... does not contain any local abstract symbol` is harmless.

---

## Skill 2: Why3 syntax and typing pitfalls

Standard Why3 syntax is assumed known. Only surprising constructs are listed.

### 1. Record literals require named fields

Write `{ bdd = bdd; size = size }`, not OCaml's `{ bdd; size }`.

### 2. Do NOT start names with an uppercase letter

Uppercase identifiers are constructors. `let lemma L1 ...` fails with a confusing
`syntax error` at the first `requires`/`ensures`. Lowercase for lemmas,
functions, variables; only modules are uppercase.

### 3. Parenthesize a comparison on the RHS of `=`

`ensures { bittest (2*x) i = (mod (2*x) (pow2 (i+1))) >= pow2 i }` fails with
`term expected`. Write `a = (b >= c)`, or better `a <-> (b >= c)` for booleans.

### 4. Declaration kinds

| Declaration | Logic? | Executable? | Typical use |
| :--- | :---: | :---: | :--- |
| `function f (x:t) : r` (no body) | ✔ | ✘ | interface, axiomatized |
| `function f ... = body` | ✔ | ✘ | logic abbreviation; usable in ghost code / `pure {}`, not in real code |
| `let function f ... = body` | ✔ | ✔ | executable and logic at once; implements both `function` and `val` |
| `let predicate f ... = body` | ✔ | ✔ | executable and logic at once; implements both `predicate` and `val` |
| `let rec function f ... variant {..}` | ✔ | ✔ | recursive executable function |
| `let rec ghost function f ... variant {..} ensures {..}` | ✔ | ghost | constructive ghost definition with proved properties (e.g. build an int bit by bit) |
| `predicate p ...` | ✔ | ✘ | never executable, even with a body |
| `let f ... = body` | ✘ | ✔ | program function |
| `let lemma l (args) requires .. ensures .. = body` | ✔ | ghost | lemma with a proof; callable as `l a b` |
| `let rec lemma l ... variant {..}` | ✔ | ghost | proof by induction |
| `lemma l: forall ...` | ✔ | — | proved statement; callable too |
| `axiom a: forall ...` | ✔ | — | assumed; callable too |

**Every lemma and axiom is callable** as a ghost function: its top-level
`forall`s (including those after an implication, e.g.
`forall x. x > 0 -> forall i. ...`) become parameters, and its hypotheses become
preconditions. So `numbits_pos1 p` is a type error (`int -> ()` expected `()`)
when the axiom also quantifies over `i`: write `numbits_pos1 p k`.

Consequences:
- To make a logic `function` callable from real code, make it `let function`.
- A `predicate` is never executable: recompute it in a `let`.
- A `let`-defined program function cannot appear in a top-level `lemma`; use a
  `let lemma` inside a program body instead.
- A `val`/`let function` keeps its **preconditions** when called in another
  definition: e.g. `let function z (x:int) = ... numbits x ...` needs
  `requires { 0 <= x }` if `val numbits` requires it.

### 5. Program vs logic name resolution; `pure { }`

When a module exports both a `val f` (program, e.g. on `int63`) and a logic
`function f` (on `int`), **in program code the `val` wins**. Symptoms:
`This expression has type int63 but is expected to have type int`, or the
reverse. Fixes:
- `pure { t }` turns a logic term into a ghost value usable as an argument:
  `mask_nonneg pure { Zbit.lsl 1 s }`, `lemma_call pure { i : int } pure { k : int }`.
- `let ghost ki = to_int k in` once, then use `ki` everywhere a lemma or logic
  function wants an `int` (`Zbit.change id ki False`, `numbits_unset_top u ki`).
- In program conditions compare like with like:
  `if Zbit.numbits id <= to_int size then ...` (not `<= size` with `size:int63`).
- Ghost logic values: `let ghost u = pure { Zbit.unset d ki } in`.

### 6. Type invariants need a witness

`type t = { ... } invariant { P }` produces a non-emptiness VC (`Goal M.t`).
Discharge it with `by { field1 = ...; field2 = ... }` (program calls such as
`ZBDD.tfalse ()` are allowed there).

### 7. Existential witnesses and int / int63

- Give the witness with `by`: `assert { exists m:int63. P m by m = n }`.
- To build an `int63` witness from an int expression in ghost code:
  `let ghost m = Int63.of_int (e) in assert { P m };` (the bound must be provable).

---

## Skill 3: Proof techniques

### 1. Work function-by-function, keep green

Implement **one** function, prove it, move on. Focused runs (`-g`).

### 2. The user is the modelling expert — stop and report

When a proof fails for a modelling reason (spec/axiom wrong or too weak), do not
silently add axioms or rewrite the model. Report the function, the failing VC
(`-X`), a counterexample if any, and the proposed fix. Small, obviously
necessary spec alignments (a missing precondition the implementation already
asserts at runtime) may be done, but must be listed explicitly in the report.

### 3. Existentials kill SMT — skolemize them into a function

Replace `exists i. id i /\ i >= s` by a total function with axioms (e.g.
`biggest_set id >= s`). A pointwise equality with an existential forces the
solver to guess witnesses.

A related trap: a predicate defined as `forall sn. exists p. P sn p` cannot be
instantiated by e-matching, because the only terms with `sn` are under the
`exists`, so no trigger exists. The goal can then fail even when the
hypothesis is literally the definition. Name the inner part:
`predicate nz sn = exists p. P sn p` and `predicate taut = forall sn. nz sn`
(trigger `nz sn`), and state local lemmas with `nz` (bddbitv.mlw
`cof_taut`/`cof_nz`).

### 4. Pointwise proofs with a local `let lemma` + case split

For `result.v = ...` (an equality of functions) or a `forall id sn. ...` goal,
write a `let lemma` **inside the body** whose `ensures` is the pointwise
statement; its parameters are concrete, so you can call ghost functions and
case-split:

```whyml
let lemma removable (id:indexes) (sn:snodes) : unit
  ensures { f b size id sn = f b' k id sn }
=
  if id < 0 then ()
  else if bittest id k then begin
    assert { g b id sn = False }
  end else begin
    ext_bittest (change id k False) id;
    assert { b'.v id sn = b.v id sn }
  end
in ...
```

The solver then closes the lambda equality (`result.v = fun id sn -> ...`) from
the pointwise lemma. This also works for preconditions of a recursive call that
are function equalities (write the lemma just before the call, with `<->`).

### 5. Extensionality must be invoked explicitly

Solvers rarely instantiate an extensionality axiom (`ext_bittest`,
`Set` extensionality, ...) on their own. Pattern:

```whyml
let ghost y = lor (land x (mask off)) (lsl (asr x off) off) in
assert { forall i. i >= 0 -> bittest y i = bittest x i };
ext_bittest y x      (* call it, like any lemma or axiom *)
```

For a bit-by-bit argument with case splits, use a local `let lemma bit (i:int)
requires { 0 <= i } ensures { bittest a i <-> bittest b i }`, then
`ext_bittest a b`.

### 6. Nonlinear arithmetic: rewrite into the exact library shape

SMT solvers do not match `mod (x - 2^s) m` with `Mod_mult: mod (x*y+z) x = mod z x`.
Assert the exact rewritten form first:

```whyml
assert { x - lsl 1 s = pow2 (j+1) * (- pow2 (s-j-1)) + x };
assert { mod (x - lsl 1 s) (pow2 (j+1)) = mod x (pow2 (j+1)) }
```

### 7. Calling lemmas instead of hoping for instantiation

When an assert does not go through, call the relevant lemma or axiom with its
arguments (`numbits_ge_pos (lsl 1 s) i`, `lor_numbits a b`): that puts the
exact instance in the context. Guard calls whose precondition may fail:
`if i > 0 then asr_numbits i half`.

---

### Context size (2026-10-16)

- `let lemma` goes into every later context; use `let ghost` facts called
  explicitly (3 to 25 times faster goals on `congruence.mlw`). See
  `why3proof.md` for the conventions of `common/`.
- A slow goal after a call: bind intermediate values with `let` before using
  their postconditions.
- Replayed hints hide failures: run a fresh proof (`fresh` in the MCP, or
  clear the hints) before a commit.

## Skill 4: Refinement (`module Concrete : Interface`)

### 1. The two-step rename

1. Rename the abstract module to `<Name>_intf` (it becomes the interface).
2. Rename the concrete module to `<Name> : <Name>_intf` and run why3find; fix
   each reported missing symbol one at a time.

### 2. What Why3 checks

- Every logic symbol of the interface must exist in the concrete module.
- Every program symbol (`val`, `let lemma`) must be implemented.
- A **defined** interface symbol can only be refined by the *same* body
  (`Cannot instantiate a defined symbol X`).
- Every interface **axiom becomes a goal** in `<Name>'impl`, checked in a context
  containing the *earlier* interface axioms (see Skill 1 §5).

### 3. Program symbols

- Interface `val f` → concrete `let f`, `let function f` or `let rec function f`.
- Prefer abstract `axiom`s in the interface over `let lemma` with a body; prove
  them as `let lemma`s in the concrete module.

### 4. Visibility for clients: only the interface is exported

`use Zbit` where `module Zbit : Zbit_intf` gives **only the names of the
interface**. Concrete-only helpers (`pow2`, `numbits_top_bit`, ...) are
`unbound` in clients. Every interface axiom is callable from clients, however
the concrete module implements it (see Skill 2 §4). If a client needs a
concrete fact, add it to the interface as an axiom and prove it in the concrete
module.

### 5. Executable logic in the concrete module

For `val test_bit ... ensures { result <-> bittest x i }` with a non-executable
`bittest` predicate, the `let test_bit` must recompute the arithmetic.

### 6. A refinement can be a *view* on an existing type

A new interface need not introduce a new type: it can declare a new abstract
logic view on an existing type (`function v (x:ZBDD.t) (s:int) (sn) : int`) with
axioms linking it to the old view, plus wrapper `val`s that keep the old
`ensures` and add new ones. The concrete module defines the view
constructively (e.g. a `let rec ghost function`) and proves the axioms.
Clients can then use whichever view makes each proof simplest.

### 7. Environment problems that look like proof failures

After a rebuild of the container, many stored proofs failed at once, even in
unchanged files. Causes and fixes:
- `~/.why3.conf` was missing: run `why3 config detect`. The default
  `memlimit = 1000` is too small for colibri2 ("Not enough heap memory",
  "Memory limit reached", and why3find says "can not calibrate prover
  colibri2"): set `memlimit = 2000` in `[main]`.
- Provers downloaded by why3find live in `~/.cache/why3find/provers` and are lost
  with the container: `why3find download cvc5` (why3find warns "prover cvc5 not
  found").
- The failing runs write their results in the cache (`.why3find/`) and replace
  the proof hints (`why3find.proofs/*.json`): restore the hints with
  `git checkout why3find.proofs/` and delete the cache entries written by the bad
  runs (`find .why3find -type f -newermt '<date>' -delete`), then rerun.
- Check a prover directly: `why3 prove -L . file.mlw -T M -G "g'vc" -P colibri2 -t 5`.
- `why3find config` rewrites `why3find.json` (formatting only): `git checkout` it.
