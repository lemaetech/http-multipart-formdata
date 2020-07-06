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

type error = [ `Invalid_content_type ]

let string_of_error = function `Invalid_content_type -> "Invalid_content_type"

(* Parse as follows - multipart/form-data; boundary=7353230 *)
let parse_content_type content_type =
  try
    let lb = Lexing.from_string ~with_positions:false content_type in
    Parser_old.parse_content_type Lexer_old.lex_multipart_header lb |> R.ok
  with _exn -> R.error @@ `Invalid_content_type

let parse_multipart_formdata http_body =
  try
    let lb = Lexing.from_string ~with_positions:false http_body in
    let lexer = Lexer_old.(lex_multipart_formdata @@ ref Multipart_formdata) in
    Parser_old.parse_multipart_formdata lexer lb |> R.ok
  with exn -> R.error @@ Printexc.to_string exn
