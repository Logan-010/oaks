(lang dune 3.23)

(name oaks)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oaks)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dns unix lwt mirage-crypto mirage-crypto-rng digestif)
 (tags
  ("add topics" "to describe" your project)))
