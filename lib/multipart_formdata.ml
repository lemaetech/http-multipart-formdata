type t = [ `File of file list | `String of string list ]

and file = {
  form_name : string;
  filename : string;
  content_type : string;
  body : bytes;
}

type error = [ `Invalid_content_type ]

let string_of_error = function `Invalid_content_type -> "Invalid_content_type"
