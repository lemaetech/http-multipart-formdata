open Sexplib
open Sexplib.Std

type t =
  | Semi
  | Eof
  | Crlf
  | Header_field_value of string
  | Token of string
  | Multipart_formdata
  | Boundary_value of string
  | Dash_bouudary_value of string
  | Close_boudary_value of string
[@@deriving sexp_of, variants]

let pp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
