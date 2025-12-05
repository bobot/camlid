(lang dune 3.19)

(generate_opam_files true)
(cram enable)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/camlid))
(license "LGPL-2.1-only")

(package
 (name camlid)
 (depends (fmt (>= 0.10.0)) (ocaml (>= 4.11))))
