# OCaml FFI rules: review checklist for generated stubs

Use it when reviewing a `tests/*/mylib_stub.c` or a new template. Each rule
says where camlid enforces it. P1 to P6 are proved for the order of the
steps of the stub (`proof/plan.mlw`); the C templates of the types are
trusted, so check them against these rules.

## GC roots (P3)

- A `value` that is live across a call that can allocate in the OCaml heap
  (`caml_alloc*`, `caml_copy_*`, a callback, `caml_failwith`...) must be
  registered: `CAMLparamN` for the arguments, `CAMLlocalN` for the locals,
  and the function returns with `CAMLreturn*`. Camlid: `code_c_fun`
  registers all boxed inputs and outputs unless the stub is noalloc.
- A helper function that allocates and has `value` locals uses
  `CAMLparam0(); CAMLlocal1(tmp); ... CAMLreturn0;`.
- `Store_field` after an allocation, never `Field(v, i) = x` on an old
  block. `Field(v, i) = x` is allowed only just after `caml_alloc_small`,
  before any other allocation.
- OCaml 5: `Field` is volatile, so copy it into a local before taking its
  address.

## `[@@noalloc]` (P3)

- The stub must not allocate, raise, or release the runtime lock, and has
  no `CAMLparam`. It returns with a plain `return`.
- Camlid: `Expert.is_noalloc`, from `ml2c_noalloc`/`c2ml_noalloc` and at
  most one result. A hand-written C function that calls the runtime needs
  `~noalloc:false`: camlid can't see it.

## Ownership (P1, P2, P5)

- Copied types: the stub frees its copy (`cty.free`) on every path,
  including the error path.
- Types owned by OCaml: ml2c borrows the C value stored in the block, c2ml
  moves a C value into a new block. The stub never frees them, and the
  finalizer does. An input and output at once is refused (`Expert.param`).
- A custom block finalizer must not allocate, raise, or call OCaml.
- An OCaml value never points to memory owned by C. Copy it
  (`copy_of_ptr`), or keep the owner alive (`keep_alive`, `parent`/`child`).
- After `dispose`, the pointer is NULL: the finalizer skips it, and the
  check of the input raises `Invalid_argument`.

## Exceptions (P6)

- `caml_failwith`/`caml_raise*` don't return: everything the stub owns must
  be freed before. Camlid: the error path frees and moves before raising.
- Conversions don't raise: refusals are checks (`Ty.checks`), run before
  any conversion for the inputs, after the call with the cleanup for the
  outputs. Fixed on 2026-10-21: a raising ml2c leaked the copies of the
  inputs converted before it.
- An exception raised by name needs `Callback.register_exception`
  (`Helper.exception_` does it). The stub checks for NULL.

## Runtime lock (P4)

- Between `caml_enter_blocking_section()` and
  `caml_leave_blocking_section()` there is no access to any `value` nor to
  the OCaml heap (`String_val`, `Bigarray` data of a non-managed array,
  `Bp_val`...). Camlid: `ml_heap` and the refusal in `code_c_fun`.

## Native and bytecode

- Unboxed (`[@unboxed]`) or untagged (`[@untagged]`) arguments, or more
  than 5 arguments, need a bytecode stub `_byte` with
  `(value *argv, int argn)` when there are more than 5. Camlid:
  `code_c_fun_bytecode`.
- The C types of the native stub must match the attributes: `double` for
  `[@unboxed] float`, `intnat` for `[@untagged] int`, `int32_t`/`int64_t`.
