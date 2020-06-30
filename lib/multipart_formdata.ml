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

type parser = {
  input_stream : char Stream.t;
  mutable current_char : char option;
}

type error =
  [ `Invalid_content_type of string
  | `Invalid_content_type_part of string
  | `Empty_content_type ]

let next_char t =
  try t.current_char <- Stream.next t.input_stream |> Option.some
  with Stream.Failure -> t.current_char <- None


let peek_char t = Stream.peek t.input_stream

let create input_stream = { input_stream; current_char = None }

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_equal = function '=' -> true | _ -> false

(*
(* Parse as follows - Content-Type: multipart/form-data; boundary=7353230 *)
let parse_content_type content_type =
  let rec parse_boundary_value s =
    let rec loop buf stream =
      match Stream.next stream with
      | '"' -> parse_part_value buf stream
      | exception Stream.Failure -> Buffer.contents
    in
    let buf = Buffer.create 10 in
    let stream = Stream.of_string s in

    ()
  in
  let find_boundary_value l =
    let rec loop = function
      | [] -> None
      | s :: tl ->
          if String.starts_with ~prefix:"boundary=" s then Some "" else loop tl
    in
    loop l
  in
  content_type |> String.split_on_char ~sep:';' |> List.map String.trim
  |> function
  | ("multipart/form-data" as ct) :: _tl -> R.ok ct
  | ct :: _ -> `Invalid_content_type ct |> R.error
  | [] -> `Empty_content_type |> R.error *)

(*
let rec parse_preamble p =
  match (p.current_char, peek_char p) with
  | Some '\r', Some '\n' -> next_char p
  | _ -> () *)
