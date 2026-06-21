(lang dune 3.7)
(cram enable)
(generate_opam_files true)

(name opickle)
(source (github TheCBaH/opickle))
(license MIT)
(authors "Vladimir N. Silyaev")
(maintainers "Vladimir N. Silyaev")

(package
 (name opickle)
 (synopsis "A Python pickle parser for OCaml")
 (description
  "opickle decodes a Python pickle byte stream (protocols 0-5) into an OCaml value tree, reading input through bytesrw. Parser only -- no serialization.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  bytesrw
  cmdliner))
