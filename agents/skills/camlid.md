# Working on camlid

Status: 2026-10-21, commit f182722.

## Architecture

| file | role |
|---|---|
| `lib/plan.ml`, `lib/plan_gen.ml` | the scheduler of the stub steps, proved in `proof/schedule.mlw` and extracted (`make extract`); never edit `plan_gen.ml` |
| `lib/expr.ml` | identifiers, variables, expressions (`{ expr : unit Fmt.t }`), definitions, and `final_print` |
| `lib/ty.ml` | the records: `mlc` (a type template), `param` (an instantiated parameter), `result` |
| `lib/expert.ml` | type constructors (strings, arrays, custom, abstract, value_type, parent/child, enum, algdata, ...), `param`, and stub generation (`code_c_fun`, `code_c_fun_bytecode`, `print_ml_fun`) |
| `lib/helper.ml` | the user API: `func`, `input`/`output`/`inout`, the common types |
| `lib/generate.ml` | headers and the files `<name>_stub.c`, `<name>.ml`, optional `.h` |

### Templates

A type `Ty.mlc` is C text that mentions two variables: `v` (the OCaml
`value`) and `cty.c` (the C value). `Expert.param` creates fresh variables
and binds the template variables to them (`Expr.binds`, printed with
`Format` stags). So one `mlc` can be used in many parameters and functions.

Fields of `mlc` that drive the stub: `conv` (`Boxed` or `Unboxable`),
`cty.init`, `cty.free`, `cty.in_call`, and `contract` (`owned_by_ml`,
`ml_heap`, and the effects `{ alloc; raises }` of ml2c and c2ml). Read their
documentation in `lib/ty.ml` before adding a type.

### Code and calls

`Expr.code`/`codef` define a C function. The variables used in the body
become its formals, except the `~locals`. `pp_call` applies formals to
expressions; formals not applied stay variables of the caller. Helper
functions usually take pointers (`value *v`, `T *c`) and are called with
`e_addr`, which is why bodies contain `*%a`.

### Printing and deduplication

`Expr.final_print` does a dry run to collect the dependencies, prints each
definition depth first before its first use, and merges C/H definitions with
the same (kind, basename, text). So a type constructor can be called many
times. Different basenames are kept (`f_nat` and `f_nativeint` stay two
stubs). ML definitions and `keep_name` ones are never merged. If two ML
definitions get the same name, OCaml reports it, so create ML types once,
outside closures.

### The stub (`Plan.schedule`, `Expert.code_c_fun`)

`Plan.schedule` works on an abstract view of the parameters (`Plan.param`)
and decides the order of the steps, the GC roots, noalloc and the
refusals. It is proved in Why3 (`proof/schedule.mlw`) against the
specification `proof/spec.mlw`: change it there, prove
(`make prove`), then `make extract`. `Expert.code_c_fun_of_plan` only prints the plan. The order is: formals, `CAMLparam`/`CAMLlocal` (omitted when noalloc), C
locals with `init_expr`, ml2c of the inputs (the one with unregistered
OCaml values last), init of the others,
`caml_enter_blocking_section`, call, leave, error check (message, move the
outputs owned by OCaml, frees, raise), c2ml of the outputs (the one with
unregistered OCaml values first), tuple, frees,
return. The bytecode stub (`_byte`) converts the unboxed arguments and calls
the native one. `print_ml_fun` writes the `external`, with `[@unboxed]`,
`[@untagged]` and `[@@noalloc]`.

## Adding a type

1. Write the `mlc` in `expert.ml`, with an honest contract
   (`Ty.copied ~ml2c ~c2ml ()` or `Ty.owned_by_ml ...`). When unsure, use
   `Ty.unknown` for the effects, and `~ml_heap:true` if the C value can point
   into an OCaml block. A conversion must not raise: refuse values with a
   check (`Ty.checks`, a C condition on `v` for the input or `c` for the
   output, which doesn't allocate). A conversion that allocates or uses
   `CAMLparam` allocates. A type built from another one (array, option,
   pointer...) must lift the checks of the inner type (`map_checks`,
   `array_checks`, `algdata_checks`).
2. Expose it in `expert.mli`, and in `helper.ml(i)` if users need it.
   Optional arguments go before labelled ones, and the order matches the
   other helpers (`?init ?clear ?hash ?compare ?mem ... ~ml ~c ()`).
3. Add or extend a test (next section), then `CHANGES` and `doc/index.mld`
   if it is user visible.

## Tests

Each `tests/<t>/` has `generator/generator.ml`, `lib.c`/`lib.h` (a fake C
library that counts its live objects), `app.ml`, `app.expected`, and the
promoted generated files `mylib.ml`/`mylib_stub.c`. The tests compare the
output in native and bytecode. The generated files are promoted silently
(`(mode (promote (until-clean)))`): after `dune runtest`, look at
`git diff tests/` to see how the stubs changed. A refactoring must give no
diff.

Traps:

- Live counts after `Gc.full_major` differ between bytecode and native
  (values kept by the bytecode stack). Print deltas, or counts at the end
  only.
- Use `Sys.opaque_identity` to keep a value alive in a test.
- valgrind with threads (`threads.posix`) reports "possibly lost" blocks for
  the tick thread: check only `definite`.
- To check that a test detects a bug, generate the wrong stub (e.g. without
  `keep_alive`) and see valgrind fail.

## Checking against FLINT

ocaml-flint (`~/ocaml-flint`) is the reference user, and must not be
modified without asking. To try a camlid change:

```sh
mkdir -p <scratch>/ws && cd <scratch>/ws
echo '(lang dune 3.0)' > dune-project
cp -r /workspaces/camlid camlid    # without _build
cp -r ~/ocaml-flint flint          # then apply the API changes there
dune build @flint/runtest
valgrind --leak-check=full --errors-for-leak-kinds=definite \
  _build/default/flint/tests/units.exe
```

Record the needed changes of flint as a patch (`flint-value_type.diff`).

## Traps of the generated C

- OCaml 5: `Field(v, i)` is volatile, and `&Field(v, i)` can't be given as
  `value *`. Copy it into a local (`Expert.ml2c_field0`).
- `caml_alloc_small` leaves the fields uninitialized: fill them before the
  next allocation.
- `Fmt.comma` breaks lines and so breaks `#define`. Use a plain `", "` in
  macros.
- `(T *)` in an OCaml comment ends it. In odoc, write `(cty *c)` or code
  blocks.
- Don't put `#include` in a definition whose name is printed: the name is
  printed too.
