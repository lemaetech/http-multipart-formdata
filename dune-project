(lang dune 2.6)

(name http-multipart-formdata)

(generate_opam_files true)

(source
 (github lemaetech/http-mutlipart-formdata))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name http-multipart-formdata)
 (synopsis "Http multipart/formdata parser")
 (tags
  ("http" "multipart" "formadata" "form" "web"))
 (description
   "OCaml implementation of RFC 7578 (Returning Values from Forms: multipart/form-data)- https://tools.ietf.org/html/rfc7578")
 (depends
  (ocaml
   (>= 4.10.0))
  dune
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  sexplib0))
