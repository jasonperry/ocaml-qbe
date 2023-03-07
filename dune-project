(lang dune 3.6)

(name qbe)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Jason Perry")

(maintainers "sh.jperry@gmail.com")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name qbe)
 (synopsis "OCaml bindings to the QBE compiler backend")
 (description "OCaml bindings to the QBE compiler backend")
 (depends ocaml dune)
; (tags
  ;  (topics "to describe" your project))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
