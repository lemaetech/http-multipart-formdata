# http-mutlipart-formdata

A libary which enables file uploads in ocaml web applications. HTTP file upload requests are generally encoded as `multipart/form-data` media content type. The library provides a simple api to parse and process such requests.

The parser implements HTTP `multipart/form-data` standard as defined in [RFC 7578](https://tools.ietf.org/html/rfc7578).

[API Documentation](https://lemaetech.co.uk/http-mutlipart-formdata/)

## Installation

```sh
$ opam install http-multipart-formdata
```

## Examples

```ocaml
let mp = Multipart.parse ~content_type_header ~body in
  let file1_1 = Multipart.Map.find "file1" mp in
  let file1_2 =
    [ { Multipart.Part.body = Bytes.of_string "\r\nContent of a.txt.\r\n\r\n"
      ; name = "file1"
      ; content_type = "text/plain"
      ; filename = Some "a.txt"
      ; parameters = Multipart.Map.empty } ]
```

[Full API Usage](https://github.com/lemaetech/http-mutlipart-formdata/blob/master/test/test.ml)
