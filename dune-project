(lang dune 2.0)
(name "nacc")

(generate_opam_files true)

(license MIT)
 (maintainers "Nathan Graule <solarliner@gmail.com>")
 (authors "Nathan Graule <solarliner@gmail.com>" "Arthur Corrensson <arthur.corrensson@gmail.com>")
(source (github codeanonorg/nacc))

 (package
  (name nacc)
  (depends
   (ocaml (>= 4.08))
   dune)
  (synopsis "Not Another Compiler Compiler")
  (description "
   nacc, short for \"Not a Compiler Compiler\" or alternatively \"Not Another Compiler Compiler\" is a homemade parser combinator, as inspired from Computerphile's video on Functional Parsing (https://www.youtube.com/watch?v=dDtZLm7HIJs).

   nacc strives to be simple yet monadic, allowing any context-free grammar to be safely parsed with this library. The library provides utility functions and operators to ease the writing of combinator parsers."))
