open Parser
open Std
open Sexplib.Std

type error =
  [ `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | Parser.error ]

type t = [ `File of file list | `String of string list ]

and file = {
  form_name : string;
  filename : string option;
  content_type : string;
  body : bytes;
}

module Params = struct
  include Map.Make (String)

  let sexp_of_t t =
    to_seq t |> Array.of_seq |> [%sexp_of: (string * string) array]

  let pp fmt t =
    sexp_of_t t |> Sexplib.Sexp.pp_hum_indent 2 fmt
    [@@ocaml.toplevel_printer] [@@warning "-32"]
end

module Content_type = struct
  type t = { ty : string; subtype : string; parameters : string Params.t }

  (* [@@deriving sexp_of] *)

  (* let pp fmt t = sexp_of_t t |> Sexplib.Sexp.pp_hum_indent 2 fmt *)

  (* let ty t = t.ty *)

  (* let subtype t = t.subtype *)

  (* let parameters t = Params.to_seq t.parameters *)

  (* let find_parameter ~name t = Params.find_opt name t.parameters *)
end

type header =
  | Content_type of Content_type.t
  | Content_disposition of string Params.t

(* [@@deriving sexp_of] *)

let content_type ct = Content_type ct

let content_disposition cd = Content_disposition cd

let is_alpha_digit = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let crlf = string "\x0D\x0A"

let is_space c = c == '\x20'

let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

let is_tspecials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '[' | ']'
  | '?' | '=' ->
      true
  | _ -> false

let is_ascii_chars = function '\x00' .. '\x7F' -> true | _ -> false

let is_vchar = function '\x21' .. '\x7E' -> true | _ -> false

let is_whitespace = function '\x20' | '\x09' -> true | _ -> false

let is_ctext = function
  | '\x21' .. '\x27' | '\x2A' .. '\x5B' | '\x5D' .. '\x7E' -> true
  | _ -> false

let is_qtext = function
  | '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' -> true
  | _ -> false

let is_token_char c =
  is_ascii_chars c
  && (not (is_space c))
  && (not (is_control c))
  && not (is_tspecials c)

let whitespace = skip_while is_whitespace

let token =
  satisfy is_token_char >>= fun ch ->
  take_while is_token_char >>= fun chars -> String.make 1 ch ^ chars |> ok

(* https://tools.ietf.org/html/rfc5322#section-3.2.1
   quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp
*)
let quoted_pair =
  char '\\' *> satisfy (fun c -> is_whitespace c || is_vchar c) >>| fun c ->
  String.make 1 '\\' ^ String.make 1 c

(* https://tools.ietf.org/html/rfc5322#section-3.2.2 *)
let fws =
  count_skip_while is_whitespace >>= fun ws_count1 ->
  count_skip_while_string 3 (fun s ->
      String.equal s "\x0D\x0A\x20" || String.equal s "\x0D\x0A\x09")
  >>= fun lws_count ->
  count_skip_while is_whitespace >>| fun ws_count2 ->
  if ws_count1 + lws_count + ws_count2 > 0 then " " else ""

let comment =
  let ctext = satisfy is_ctext >>| String.make 1 in
  let rec loop_comment () =
    char '('
    *> many
         ( fws >>= fun sp ->
           ctext
           <|> quoted_pair
           <|> (loop_comment () >>| fun txt -> "(" ^ txt ^ ")")
           >>| ( ^ ) sp )
    >>| String.concat ~sep:""
    >>= fun comment_text ->
    fws >>= fun sp -> ok @@ comment_text ^ sp <* char ')'
  in
  loop_comment ()

let cfws =
  many
    ( fws >>= fun sp ->
      comment >>| fun comment_text -> sp ^ comment_text )
  >>= (fun l ->
        fws >>| fun sp -> if String.length sp > 0 then l @ [ sp ] else l)
  <|> (fws >>| fun sp -> if String.length sp > 0 then [ sp ] else [])

let quoted_string =
  let qcontent = satisfy is_qtext >>| String.make 1 <|> quoted_pair in
  cfws *> char '"' *> many (fws >>= fun sp -> qcontent >>| ( ^ ) sp)
  >>| String.concat ~sep:""
  >>= fun q_string -> fws >>| (fun sp -> q_string ^ sp) <* char '"'

let param_value = token <|> quoted_string

let param =
  whitespace *> char ';' *> whitespace *> token >>= fun attribute ->
  char '=' *> param_value >>| fun value -> (attribute, value)

let restricted_name =
  let is_restricted_name_chars = function
    | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
    | c when is_alpha_digit c -> true
    | _ -> false
  in
  satisfy is_alpha_digit >>= fun first_ch ->
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch;
  take_while_n 126 is_restricted_name_chars >>= fun restricted_name ->
  Buffer.add_string buf restricted_name;
  ok @@ Buffer.contents buf

let content_disposition =
  crlf
  *> string "Content-Disposition"
  *> char ':'
  *> whitespace
  *> string "form-data"
  *> many param
  >>| (List.to_seq >> Params.of_seq >> content_disposition)

let content_type parse_header_name =
  ( if parse_header_name then crlf *> string "Content-Type" *> char ':' *> ok ()
  else ok () )
  *> whitespace
  *> restricted_name
  >>= fun ty ->
  char '/' *> restricted_name >>= fun subtype ->
  whitespace *> many param >>| fun params ->
  let parameters = params |> List.to_seq |> Params.of_seq in
  { Content_type.ty; subtype; parameters } |> content_type

let boundary_header, boundary =
  let is_bcharnospace = function
    | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?' ->
        true
    | c when is_alpha_digit c -> true
    | _ -> false
  in
  let is_bchars = function
    | '\x20' -> true
    | c when is_bcharnospace c -> true
    | _ -> false
  in
  let boundary =
    take_while_n 69 is_bchars >>= fun bchars ->
    satisfy is_bcharnospace >>| fun last_char ->
    bchars ^ String.make 1 last_char
  in
  let boundary_header = char '"' *> boundary <* char '"' <|> token in
  (boundary_header, boundary)

let multipart_formdata_header =
  let param =
    whitespace *> char ';' *> whitespace *> token >>= fun attribute ->
    (char '=' *> if attribute = "boundary" then boundary_header else param_value)
    >>| fun value -> (attribute, value)
  in
  whitespace
  *> (string "multipart/form-data" >>*? `Not_multipart_formdata_header)
  *> whitespace
  *> many param
  >>= fun params -> params |> List.to_seq |> Params.of_seq |> ok

let multipart_bodypart boundary_value =
  crlf *> string "--" *> boundary >>= fun _boundary_value ->
  many
    ( content_type true
    <|> content_disposition
    <|> fail
          (`Invalid_multipart_body_header
            "Only Content-Type and Content-Disposition header supported.") )
  >>= fun _part_headers ->
  many (not_string ("\r\n--" ^ boundary_value)) >>| String.concat ~sep:""

let parse ~header ~body =
  let open R.O in
  let* header_params = parse (`String header) multipart_formdata_header in
  match Params.find_opt "boundary" header_params with
  | Some boundary_value -> parse body (many (multipart_bodypart boundary_value))
  | None -> R.error `Boundary_parameter_not_found
