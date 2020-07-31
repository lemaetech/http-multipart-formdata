open Reparse
open Sexplib0
open Sexplib0.Sexp_conv
module String = StringLabels

let ( let* ) = Result.bind

module R = Result

type error =
  [ `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | `Name_parameter_not_found
  | Reparse.error ]
[@@deriving sexp_of]

module String_map = struct
  include Map.Make (String)

  let union a b = union (fun _key a _b -> Some a) a b

  let sexp_of_t f t =
    let s = sexp_of_pair sexp_of_string f in
    let l = to_seq t |> List.of_seq in
    sexp_of_list s l

  let pp f fmt t =
    sexp_of_t f t |> Sexp.pp_hum_indent 2 fmt
    [@@ocaml.toplevel_printer] [@@warning "-32"]
end

type t = {
  form_field : string;
  filename : string option;
  content_type : string;
  parameters : string String_map.t;
  body : bytes;
}
[@@deriving sexp_of]

let form_field t = t.form_field

let filename t = t.filename

let content_type t = t.content_type

let is_file t = Option.is_some t.filename

let body t = t.body

let pp fmt t = Sexp.pp_hum_indent 2 fmt (sexp_of_t t)

type part_header =
  | Content_type of {
      ty : string;
      subtype : string;
      parameters : string String_map.t;
    }
  | Content_disposition of string String_map.t

let is_alpha_digit = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

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

let p_whitespace = skip_while is_whitespace

let p_token =
  satisfy is_token_char >>= fun ch ->
  take_while is_token_char >>= fun chars -> String.make 1 ch ^ chars |> ok

(* https://tools.ietf.org/html/rfc5322#section-3.2.1
   quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp
*)
let p_quoted_pair =
  char '\\' *> satisfy (fun c -> is_whitespace c || is_vchar c) >>| fun c ->
  String.make 1 '\\' ^ String.make 1 c

(* https://tools.ietf.org/html/rfc5322#section-3.2.2 *)
let p_fws =
  count_skip_while is_whitespace >>= fun ws_count1 ->
  count_skip_while_string 3 (fun s ->
      String.equal s "\x0D\x0A\x20" || String.equal s "\x0D\x0A\x09")
  >>= fun lws_count ->
  count_skip_while is_whitespace >>| fun ws_count2 ->
  if ws_count1 + lws_count + ws_count2 > 0 then " " else ""

let p_comment =
  let ctext = satisfy is_ctext >>| String.make 1 in
  let rec loop_comment () =
    char '('
    *> many
         ( p_fws >>= fun sp ->
           ctext
           <|> p_quoted_pair
           <|> (loop_comment () >>| fun txt -> "(" ^ txt ^ ")")
           >>| ( ^ ) sp )
    >>| String.concat ~sep:""
    >>= fun comment_text ->
    p_fws >>= fun sp -> ok @@ comment_text ^ sp <* char ')'
  in
  loop_comment ()

let p_cfws =
  many
    ( p_fws >>= fun sp ->
      p_comment >>| fun comment_text -> sp ^ comment_text )
  >>= (fun l ->
        p_fws >>| fun sp -> if String.length sp > 0 then l @ [ sp ] else l)
  <|> (p_fws >>| fun sp -> if String.length sp > 0 then [ sp ] else [])

let p_quoted_string =
  let qcontent = satisfy is_qtext >>| String.make 1 <|> p_quoted_pair in
  p_cfws *> char '"' *> many (p_fws >>= fun sp -> qcontent >>| ( ^ ) sp)
  >>| String.concat ~sep:""
  >>= fun q_string -> p_fws >>| (fun sp -> q_string ^ sp) <* char '"'

let p_param_value = p_token <|> p_quoted_string

let p_param =
  p_whitespace *> char ';' *> p_whitespace *> p_token >>= fun attribute ->
  char '=' *> p_param_value >>| fun value -> (attribute, value)

let p_restricted_name =
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

let p_content_disposition =
  string "Content-Disposition"
  *> char ':'
  *> p_whitespace
  *> string "form-data"
  *> many p_param
  >>| fun params ->
  let params = List.to_seq params |> String_map.of_seq in
  Content_disposition params

let p_content_type parse_header_name =
  ( if parse_header_name then string "Content-Type" *> char ':' *> ok ()
  else ok () )
  *> p_whitespace
  *> p_restricted_name
  >>= fun ty ->
  char '/' *> p_restricted_name >>= fun subtype ->
  p_whitespace *> many p_param >>| fun params ->
  let parameters = params |> List.to_seq |> String_map.of_seq in
  Content_type { ty; subtype; parameters }

let p_header_boundary =
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
  let is_dquote = function '"' -> true | _ -> false in
  let boundary =
    take_while_n 70 is_bchars >>= fun bchars ->
    let len = String.length bchars in
    if len > 0 then
      let last_char = String.unsafe_get bchars (len - 1) in
      if is_bcharnospace last_char then ok bchars
      else fail `Invalid_last_char_boundary_value
    else fail `Zero_length_boundary_value
  in
  char_if is_dquote *> boundary <* char_if is_dquote <|> p_token

let p_multipart_formdata_header =
  let param =
    p_whitespace *> char ';' *> p_whitespace *> p_token >>= fun attribute ->
    ( char '='
    *> if attribute = "boundary" then p_header_boundary else p_param_value )
    >>| fun value -> (attribute, value)
  in
  ( string_if "\r\n"
    *> string_if "Content-Type:"
    *> p_whitespace
    *> string "multipart/form-data"
  >>*? `Not_multipart_formdata_header )
  *> p_whitespace
  *> many param
  >>= fun params -> params |> List.to_seq |> String_map.of_seq |> ok

let body_part headers body =
  let name, content_type, filename, parameters =
    List.fold_left
      (fun (name, ct, filename, params) header ->
        match header with
        | Content_type ct ->
            let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
            (name, content_type, filename, String_map.union params ct.parameters)
        | Content_disposition params2 ->
            let name = String_map.find_opt "name" params2 in
            let filename = String_map.find_opt "filename" params2 in
            (name, ct, filename, String_map.union params params2))
      (None, None, None, String_map.empty)
      headers
  in
  match name with
  | None -> Reparse.fail `Name_parameter_not_found
  | Some nm ->
      let content_type = try Option.get content_type with _ -> "text/plain" in
      Reparse.ok
        {
          form_field = nm;
          filename;
          content_type;
          parameters;
          body = Bytes.unsafe_of_string body;
        }

let p_multipart_bodyparts boundary_value =
  let dash_boundary = "--" ^ boundary_value in
  let rec loop_body buf =
    line >>= function
    | Some ln ->
        if ln <> dash_boundary then (
          Buffer.add_string buf (ln ^ "\r\n");
          loop_body buf )
        else (Buffer.contents buf, Some ln) |> ok
    | None -> (Buffer.contents buf, None) |> ok
  in
  let rec loop_parts parts = function
    | Some ln ->
        if ln = dash_boundary ^ "--" then ok parts
        else if ln = dash_boundary then
          many (string "\r\n" *> p_content_type true <|> p_content_disposition)
          >>= fun headers ->
          loop_body (Buffer.create 5) >>= fun (body, ln) ->
          body_part headers body >>= fun bp -> loop_parts (bp :: parts) ln
        else line >>= loop_parts parts
    | None -> ok parts
  in
  line >>= loop_parts [] >>= fun parts -> List.rev parts |> ok

let parse ~header ~body =
  let* header_params = parse (`String header) p_multipart_formdata_header in
  match String_map.find_opt "boundary" header_params with
  | Some boundary_value -> parse body (p_multipart_bodyparts boundary_value)
  | None -> Result.error `Boundary_parameter_not_found
