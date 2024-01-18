(lang dune 3.4)

(name lambda-cube)

(generate_opam_files true)

(source
 (github coetaur0/lambda-cube))

(authors "Aurelien Coet")

(maintainers "Aurelien Coet")

(license MIT)

(package
 (name lambda-cube)
 (synopsis "Implementations of type systems on the lambda cube.")
 (depends ocaml dune ppx_deriving core core_unix menhir elpi alcotest))

(using menhir 2.1)
