module R = struct
  include Result

  module O = struct
    let ( let* ) = bind

    let ( >>= ) = ( let* )

    let ( let+ ) x f = map f x

    let ( >>| ) = ( let+ )
  end
end

let[@inline] ( >> ) f g x = g (f x)

module String = struct
  include StringLabels

  let starts_with ~prefix s =
    let prefix_len = length prefix in
    let full_len = length s in
    full_len >= prefix_len && String.equal (sub s ~pos:0 ~len:prefix_len) prefix


  let chop_prefix ~prefix s =
    let prefix_len = length prefix in
    let full_len = length s in
    if
      full_len >= prefix_len
      && String.equal (sub s ~pos:0 ~len:prefix_len) prefix
    then sub s ~pos:prefix_len ~len:(full_len - prefix_len)
    else s
end

type t = [ `File of file list | `String of string list ]

and file = {
  form_name : string;
  filename : string;
  content_type : string;
  body : bytes;
}

open Sexplib.Std

type error = [ `Invalid_content_type of string | `Invalid_boundary_value ]
[@@deriving sexp_of]

type ('a, 'e) result = ('a, 'e) Result.t = Ok of 'a | Error of 'e
[@@deriving sexp_of]

let sexp_of_result = sexp_of_result sexp_of_string sexp_of_error

(* Parse as follows - multipart/form-data; boundary=7353230 *)
let parse_content_type content_type =
  let parse_boundary_value s =
    try Lexing.from_string s |> Lexer.lex_boundary |> R.ok
    with _exn -> R.error `Invalid_boundary_value
  in
  let prefix = "multipart/form-data;" in
  if String.starts_with ~prefix content_type then
    String.chop_prefix ~prefix content_type |> parse_boundary_value
  else R.error @@ `Invalid_content_type content_type
