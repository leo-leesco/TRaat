(lang dune 3.17)

(name traat)

(generate_opam_files true)

(source
 (github leo-leesco/TRaat))

(authors "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(maintainers "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(package
 (name traat)
 (synopsis "OCaml implementation of algorithms as described in Term rewriting and all that")
 (depends ocaml ounit2)
 (tags
  ("ocaml" "term rewriting" "unification" "matching")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
