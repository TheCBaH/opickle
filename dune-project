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
  "opickle reads Python pickle streams (protocols 0-5) through bytesrw and returns a structural OCaml value graph. Reconstruction calls are represented as data.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  bytesrw
  cmdliner
  (ppx_expect :with-test)
  (conf-python-3 :with-test)))
