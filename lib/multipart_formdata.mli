type file

type t = [ `File of file list | `String of string list ]

type error =
  [ `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | Parser.error ]

type header

val parse :
  header:string ->
  body:Parser.src ->
  ((header list * string) list, [> error ]) result
