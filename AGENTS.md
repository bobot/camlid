# Instructions for coding agents

Camlid generates C stubs and OCaml externals from a description of a C
library written in OCaml (`lib/`). The generated code must be memory safe:
it follows the OCaml FFI rules, and handles C values like C++ objects
(constructor, destructor, copy, move).

## Goals of camlid

- **Helper** (`lib/helper.ml`): safe helpers. In simple cases the user does
  the right thing without knowing the FFI rules.
- **Expert** (`lib/expert.ml`): tools to define new types. They are correct
  when used as documented, and refuse wrong combinations at generation time
  (`Invalid_argument "Camlid: ..."`) instead of generating a wrong stub.
- Hacks stay possible: the user can write any C code in a formatter
  (`Expr.expr "..."`). Don't remove that to make things safer.
- Concise documentation (`doc/index.mld`), centred on the memory lifecycle.
- The reference user is the FLINT binding (`~/ocaml-flint`). Check that
  API changes still work there (see `agents/skills/camlid.md`).

## Commands

| Task | Command |
|---|---|
| build | `dune build` |
| tests (native and bytecode, golden files) | `dune runtest` (= `make test`) |
| accept new generated stubs or outputs | `dune promote` (review the diff of `tests/*/mylib_stub.c` first) |
| formatting | `dune fmt` (ocamlformat 0.29.0) |
| documentation, must give no warnings | `dune build @doc` |
| memory check of a test | `valgrind --leak-check=full --errors-for-leak-kinds=definite --error-exitcode=1 _build/default/tests/<t>/app.exe` |

The minimum OCaml version is 4.11: no `String.starts_with`, `In_channel`,
`Seq` functions newer than 4.11, and so on. The generated C is C11 and is
compiled with `-Wall -Werror -Wconversion --pedantic-errors` in the tests.

## Knowledge base: `agents/`

- `skills/camlid.md`: architecture (Expr, Ty, Expert, Helper, Generate),
  how a type template becomes a stub, how to add a type or a test, and the
  traps.
- `skills/ocaml-ffi.md`: the OCaml FFI rules that the generated code must
  follow. Use it as the checklist when reviewing a generated stub.
- `skills/why3.md`: proving with Why3/why3find (copied from colibrics).
- `design/safe-stubs.md`: proposal for a stub plan IR proved in Why3, a
  typed C API, and the three API levels.

Update these files when the code they describe changes. Write plain, short
English, with dates and commit ids for status statements.

## Rules

- Every new type or option gets a test in `tests/` with an `app.ml` that
  exercises the finalizers (`Gc.full_major`) and counts the live C objects.
  Run valgrind on it.
- Look at the generated stub, not only at the test result. A stub can pass
  the tests and still break an FFI rule (an unrooted `value`, an allocation
  in a `[@@noalloc]` external).
- Prefer refusing a combination with a clear message over generating a stub
  that is only sometimes correct.
- Keep `CHANGES` (unreleased section) up to date with user-visible changes.
- Commit only the files you changed. Avoid `git commit -a`: the working tree
  may contain someone else's work (`.devcontainer/`, `.vscode/`, patches).
  Don't push without asking.
