open Sexplib.Std

type multipart_formdata = {
  parts : multipart_body_part list;
  close_boundary : string;
}
[@@deriving sexp_of]

and multipart_body_part = {
  dash_boundary : string;
  headers : multipart_body_header list;
  body : string option;
}
[@@deriving sexp_of]

and multipart_body_header = {
  header : multipart_body_header_type;
  params : (string * string) list;
}
[@@deriving sexp_of]

and multipart_body_header_type =
  [ `Content_type of string | `Content_disposition of string ]
[@@deriving sexp_of]
