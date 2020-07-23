type file

type t = [ `File of file list | `String of string list ]

type error =
  private
  [> `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | Reparse.error ]

type header

val parse :
  header:string ->
  body:Reparse.input ->
  ((header list * string) list, error) result
