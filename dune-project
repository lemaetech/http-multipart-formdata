(lang dune 2.6)

(using menhir 2.0)

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
 (synopsis
   "Ocaml implementation of RFC 7578 (Returning Values from Forms: multipart/form-data)- https://tools.ietf.org/html/rfc7578")
 (depends
  (ocaml
   (>= 4.10.0))
  (dune
   (and
    :build
    (>= 2.6.0)))))
