(lang dune 3.23)

(name oaks)

(generate_opam_files true)

(source
 (github logan-010/oaks))

(authors "Logan B")

(maintainers "Logan B")

(license MIT)

(package
 (name oaks)
 (synopsis "LAN file share.")
 (description "A simple and secure LAN file sharing program.")
 (depends ocaml dns unix lwt mirage-crypto mirage-crypto-rng)
 (tags
  ("mDNS" "share" "cryptography")))
