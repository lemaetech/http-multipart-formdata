(lang dune 2.6)

(name http-multipart-formdata)

(version 1.0.0)

(generate_opam_files true)

(source
 (github bikallem/http-mutlipart-formdata))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name http-multipart-formdata)
 (synopsis "Http multipart/formdata parser")
 (tags
  ("http" "multipart" "formadata" "form" "web"))
 (description
   "Ocaml implementation of RFC 7578 (Returning Values from Forms: multipart/form-data)- https://tools.ietf.org/html/rfc7578")
 (depends
  (ocaml
   (>= 4.10.0))
  dune
  reparse
  ppx_sexp_conv
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  bigstringaf
  sexplib0))
