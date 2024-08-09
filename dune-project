(lang dune 3.16)

(name programme_generator)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name programme_generator)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   (utop :dev)
   (ocaml-lsp-server (and :dev (>= 1.19.0)))
   )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
