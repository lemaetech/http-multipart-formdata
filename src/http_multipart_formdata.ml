(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
open! Reparse_lwt.Stream

module Map = struct
  include Map.Make (String)

  let pp pp_value fmt t =
    let pp_kv = Fmt.pair ~sep:Fmt.comma Fmt.string pp_value in
    Fmt.seq ~sep:Fmt.semi pp_kv fmt (to_seq t)
end

module Part_header = struct
  type t =
    { name : string
    ; content_type : string
    ; filename : string option
    ; parameters : string Map.t
    }

  let name t = t.name

  let content_type t = t.content_type

  let filename t = t.filename

  let param_value name t = Map.find_opt name t.parameters

  let compare (a : t) (b : t) = compare a b

  let equal (a : t) (b : t) = compare a b = 0

  let pp fmt t =
    let fields =
      [ Fmt.field "name" (fun p -> p.name) Fmt.string
      ; Fmt.field "content_type" (fun p -> p.content_type) Fmt.string
      ; Fmt.field "filename" (fun p -> p.filename) Fmt.(option string)
      ; Fmt.field "parameters" (fun p -> p.parameters) (Map.pp Fmt.string)
      ]
    in
    Fmt.record ~sep:Fmt.semi fields fmt t
end

let is_alpha_digit = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
    true
  | _ -> false

let is_space c = c == '\x20'

let is_control = function
  | '\x00' .. '\x1F'
  | '\x7F' ->
    true
  | _ -> false

let is_tspecial = function
  | '('
  | ')'
  | '<'
  | '>'
  | '@'
  | ','
  | ';'
  | ':'
  | '\\'
  | '"'
  | '/'
  | '['
  | ']'
  | '?'
  | '=' ->
    true
  | _ -> false

let is_ascii_char = function
  | '\x00' .. '\x7F' -> true
  | _ -> false

let is_qtext = function
  | '\x21'
  | '\x23' .. '\x5B'
  | '\x5D' .. '\x7E' ->
    true
  | _ -> false

let is_token_char c =
  is_ascii_char c
  && (not (is_space c))
  && (not (is_control c))
  && not (is_tspecial c)

let implode l = List.to_seq l |> String.of_seq

let token =
  let+ chars = take ~at_least:1 (char_if is_token_char) <?> "[token]" in
  implode chars

(* https://tools.ietf.org/html/rfc5322#section-3.2.1 quoted-pair = ('\' (VCHAR /
   WSP)) / obs-qp *)
let quoted_pair = String.make 1 <$> char '\\' *> (whitespace <|> vchar)

let quoted_string =
  let qtext = String.make 1 <$> char_if is_qtext in
  let qcontent =
    (fun l -> String.concat "" l) <$> take (qtext <|> quoted_pair)
  in
  dquote *> qcontent <* dquote

let param_value = token <|> quoted_string

let param =
  let name = skip whitespace *> char ';' *> skip whitespace *> token in
  let value = char '=' *> param_value in
  map2 (fun name value -> (name, value)) name value

let p_restricted_name =
  let p_restricted_name_chars =
    char_if (function
      | '!'
      | '#'
      | '$'
      | '&'
      | '-'
      | '^'
      | '_'
      | '.'
      | '+' ->
        true
      | c when is_alpha_digit c -> true
      | _ -> false)
  in
  let* first_ch = char_if is_alpha_digit in
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch;
  let+ restricted_name = take ~up_to:126 p_restricted_name_chars in
  Buffer.add_string buf (implode restricted_name);
  Buffer.contents buf

type boundary = string

let parse_boundary ~content_type =
  let boundary =
    let is_bcharnospace = function
      | '\''
      | '('
      | ')'
      | '+'
      | '_'
      | ','
      | '-'
      | '.'
      | '/'
      | ':'
      | '='
      | '?' ->
        true
      | c when is_alpha_digit c -> true
      | _ -> false
    in
    let bchars =
      char_if (function
        | '\x20' -> true
        | c when is_bcharnospace c -> true
        | _ -> false)
    in
    let boundary =
      let* bchars = take ~up_to:70 bchars in
      let len = List.length bchars in
      if len > 0 then
        let last_char = List.nth bchars (len - 1) in
        if is_bcharnospace last_char then
          return (implode bchars)
        else
          fail "Invalid boundary value: invalid last char"
      else
        fail "Invalid boundary value: 0 length"
    in
    optional dquote *> boundary <* optional dquote <|> token
  in
  let param =
    let* attribute = skip whitespace *> char ';' *> skip whitespace *> token in
    let+ value =
      char '='
      *>
      if attribute = "boundary" then
        boundary
      else
        param_value
    in
    (attribute, value)
  in
  let boundary_parser =
    skip whitespace
    *> (string_cs "multipart/form-data" <?> "Not multipart formdata header")
    *> skip whitespace
    *> take param
    >>= fun params ->
    match List.assoc_opt "boundary" params with
    | Some b -> return b
    | None -> fail "'boundary' parameter not found"
  in
  let input = input_of_stream (Lwt_stream.of_string content_type) in
  parse boundary_parser input

type part_body_header =
  | Content_type of
      { ty : string
      ; subtype : string
      ; parameters : string Map.t
      }
  | Content_disposition of string Map.t

let content_disposition =
  let+ params =
    string_cs "Content-Disposition:"
    *> skip whitespace
    *> string_cs "form-data"
    *> take param
  in
  let params = List.to_seq params |> Map.of_seq in
  Content_disposition params

let content_type parse_header_name =
  let* ty =
    (if parse_header_name then
      string_cs "Content-Type:" *> unit
    else
      unit)
    *> skip whitespace
    *> p_restricted_name
  in
  let* subtype = char '/' *> p_restricted_name in
  let+ params = take param in
  let parameters = params |> List.to_seq |> Map.of_seq in
  Content_type { ty; subtype; parameters }

let part_body_header =
  take ~at_least:1 ~sep_by:crlf (any [ content_disposition; content_type true ])
  >>= fun headers ->
  let name, content_type, filename, parameters =
    List.fold_left
      (fun (name, ct, filename, params) header ->
        match header with
        | Content_type ct ->
          let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
          ( name
          , content_type
          , filename
          , Map.union (fun _key a _b -> Some a) params ct.parameters )
        | Content_disposition params2 ->
          let name = Map.find_opt "name" params2 in
          let filename = Map.find_opt "filename" params2 in
          ( name
          , ct
          , filename
          , Map.union (fun _key a _b -> Some a) params params2 ))
      (None, None, None, Map.empty)
      headers
  in
  match name with
  | None -> fail "Invalid part. parameter 'name' not found"
  | Some name ->
    let content_type = Option.value content_type ~default:"text/plain" in
    let parameters = Map.remove "name" parameters in
    let parameters =
      match filename with
      | Some _ -> Map.remove "filename" parameters
      | None -> parameters
    in
    return { Part_header.name; content_type; filename; parameters }

let parse ?(part_body_buf_size = 1024) ~boundary ~on_part http_body =
  let p =
    let boundary_type =
      let body_end = string_cs "--\r\n" $> `Body_end in
      let part_start = string_cs "\r\n" $> `Part_start in
      body_end <|> part_start <?> "Invalid 'multipart/formdata' boundary value"
    in
    let crlf_dash_boundary = string_cs @@ Format.sprintf "\r\n--%s" boundary in
    let rec loop_parts () =
      crlf_dash_boundary *> boundary_type
      >>= function
      | `Body_end -> unit
      | `Part_start ->
        part_body_header
        >>= fun header ->
        let part_body_stream, pusher =
          Lwt_stream.create_bounded part_body_buf_size
        in
        of_promise (on_part header part_body_stream)
        >>= fun () ->
        take_while_cbp
          ~while_:(is_not crlf_dash_boundary)
          ~on_take_cb:(fun x -> pusher#push x)
          any_char
        *> loop_parts ()
    in
    (*** Ignore preamble - any text before first boundary value. ***)
    take_while_cb
      ~while_:(is_not crlf_dash_boundary)
      ~on_take_cb:(fun (_ : char) -> ())
      any_char
    *> loop_parts ()
  in
  let input = input_of_stream http_body in
  parse p input
