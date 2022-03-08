(lang dune 3.0)
(name ppx_ts)
(version 0.1.6)

(generate_opam_files true)

(source (github green-labs/ppx_ts))
(license MIT)
(authors "Greenlabs Dev")
(maintainers "developer@greenlabs.co.kr")

(package
 (name ppx_ts)
 (synopsis "A PPX helps binding to typescript modules")
 (description "")
 (depends
  (ocaml (>= 4.12.1))
  (dune (>= 3.0))
  (ppxlib (>= 0.22.0))
  ppx_inline_test
  ppx_expect
  ppx_deriving))
