(lang dune 2.7)
(generate_opam_files true)

(name tezos-context-hash)
(source (github tarides/tezos-context-hash))
(license MIT)
(maintainers "Tarides <contact@tarides.com>")
(authors
 "Clément Pascutto <clement@pascutto.fr>"
 "Mattias Roux <mattias@tarides.com>"
 "Thomas Gazagnaire <thomas@tarides.com>")

(package
 (name tezos-context-hash)
 (synopsis "Specification of the Tezos context hash")
 (description "\
This package contains the specification of the Tezos context hash
as well as generators of datasets useful to verify that an implementation
complies with the specification.
")
 (depends
  (digestif (>= 0.9.0))
  (fmt (>= 0.8.7))
  (repr (>= "0.2.0"))
  zarith))

(package
 (name tezos-context-hash-irmin)
 (synopsis "Irmin implementation of the Tezos context hash specification")
 (description "Irmin implementation of the Tezos context hash specification")
 (depends
  (alcotest :with-test)
  tezos-context-hash
  (irmin (and (>= 2.7.2) (< 2.8.0)))
  cmdliner
  fmt
  yojson
  irmin-pack
  ppx_irmin))
