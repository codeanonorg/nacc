(lang dune 2.0)
(name "nacc")
(version 0.1)

(generate_opam_files true)

(license MIT)
(maintainers "Nathan Graule <solarliner@gmail.com>")
(authors "Nathan Graule <solarliner@gmail.com>" "Arthur Correnson <arthur.correnson@gmail.com>")
(source (github codeanonorg/nacc))
(bug_reports https://github.com/codeanonorg/nacc/issues)
(documentation https://codeanonorg.github.io/nacc)
(homepage https://github.com/codeanonorg/nacc)

(package
  (name nacc)
  (depends
    (ocaml (>= 4.08))
    (ppx_deriving (>= 4.4))
    (ppx_variants_conv (>= 0.13))
    (dune (>= 2.0)))
  (synopsis "Not Another Compiler Compiler")
  (description "
    nacc, short for \"Not a Compiler Compiler\" or alternatively \"Not Another Compiler Compiler\" is a homemade parser combinator, as inspired from Computerphile's video on Functional Parsing (https://www.youtube.com/watch?v=dDtZLm7HIJs).

    nacc strives to be simple yet monadic, allowing any context-free grammar to be safely parsed with this library. The library provides utility functions and operators to ease the writing of combinator parsers."))
