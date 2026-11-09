(lang dune 3.19)

(generate_opam_files true)
(cram enable)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/camlid))
(license "LGPL-2.1-only")
(name camlid)

(package
 (name camlid)
 (synopsis "Generate the C stubs of a C library from a description in OCaml")
 (description
  "Camlid generates the C stubs and the OCaml externals of a C library from a description of its functions written in OCaml. The C values are handled like C++ objects (constructor, destructor, copy), and the generated code follows the rules of the OCaml FFI.")
 (depends (fmt (>= 0.10.0)) (ocaml (>= 4.14))))
