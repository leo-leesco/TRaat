(lang dune 3.17)

(name unification)

(generate_opam_files true)

(source
 (github leo-leesco/TRaat-unification))

(authors "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(maintainers "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(package
 (name unification)
 (synopsis "OCaml implementation of unification as described in Term rewriting and all that")
 (depends ocaml ounit2)
 (tags
  ("ocaml" "term rewriting" "unification")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
