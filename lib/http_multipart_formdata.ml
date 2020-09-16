(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
open Reparse
open Sexplib0
open Sexplib0.Sexp_conv

exception Http_multipart_formdata of string

module String_map = struct
  include Map.Make (String)

  let sexp_of_t f t =
    let s = sexp_of_pair sexp_of_string f in
    let l = to_seq t |> List.of_seq in
    sexp_of_list s l

  let pp f fmt t =
    sexp_of_t f t |> Sexp.pp_hum_indent 2 fmt
    [@@ocaml.toplevel_printer] [@@warning "-32"]
end

module File_part = struct
  type t =
    { filename : string option
    ; content_type : string
    ; parameters : string String_map.t
    ; body : bytes }
  [@@deriving sexp_of]

  let filename t = t.filename
  let content_type t = t.content_type
  let body t = t.body
  let find_parameter nm t = String_map.find_opt nm t.parameters
  let pp fmt t = Sexp.pp_hum_indent 2 fmt (sexp_of_t t)
end

type t = part list String_map.t [@@deriving sexp_of]

and part =
  | File   of File_part.t
  | String of string
[@@deriving sexp_of]

let pp fmt t = Sexp.pp_hum_indent 2 fmt (sexp_of_t t)

type part_header =
  | Content_type        of
      { ty : string
      ; subtype : string
      ; parameters : string String_map.t }
  | Content_disposition of string String_map.t

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

let is_tspecials = function
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

let is_ascii_chars = function
  | '\x00' .. '\x7F' -> true
  | _                -> false

let is_vchar = function
  | '\x21' .. '\x7E' -> true
  | _                -> false

let is_whitespace = function
  | '\x20'
  | '\x09' ->
      true
  | _ -> false

let is_ctext = function
  | '\x21' .. '\x27'
  | '\x2A' .. '\x5B'
  | '\x5D' .. '\x7E' ->
      true
  | _ -> false

let is_qtext = function
  | '\x21'
  | '\x23' .. '\x5B'
  | '\x5D' .. '\x7E' ->
      true
  | _ -> false

let is_token_char c =
  is_ascii_chars c
  && (not (is_space c))
  && (not (is_control c))
  && not (is_tspecials c)

let p_whitespace = skip whitespace
let implode l = List.to_seq l |> String.of_seq
let f a () = a

let p_token =
  satisfy is_token_char
  >>= fun ch ->
  many (satisfy is_token_char)
  >|= fun (_, chars) -> String.make 1 ch ^ implode chars

(* https://tools.ietf.org/html/rfc5322#section-3.2.1
   quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp
*)
let p_quoted_pair =
  char '\\' *> satisfy (fun c -> is_whitespace c || is_vchar c)
  >|= fun c -> String.make 1 '\\' ^ String.make 1 c

(* https://tools.ietf.org/html/rfc5322#section-3.2.2 *)
let p_fws =
  skip p_whitespace
  >>= fun ws_count1 ->
  skip (string "\x0D\x0A\x20" <|> delay (f (string "\x0D\x0A\x09")))
  >>= fun lws_count ->
  skip p_whitespace
  >|= fun ws_count2 -> if ws_count1 + lws_count + ws_count2 > 0 then " " else ""

let p_comment =
  let ctext = satisfy is_ctext >|= String.make 1 in
  let rec loop_comment () =
    char '('
    *> many
         ( p_fws
         >>= fun sp ->
         ctext
         <|> delay (f p_quoted_pair)
         <|> delay (f (loop_comment () >|= fun txt -> "(" ^ txt ^ ")"))
         >|= ( ^ ) sp )
    >|= (fun (_, chars) -> String.concat "" chars)
    >>= fun comment_text ->
    p_fws >>= fun sp -> return (comment_text ^ sp) <* char ')'
  in
  loop_comment ()

let p_cfws =
  many (p_fws >>= fun sp -> p_comment >|= fun comment_text -> sp ^ comment_text)
  >>= (fun (_, l) ->
        p_fws >|= fun sp -> if String.length sp > 0 then l @ [sp] else l)
  <|> (p_fws >|= fun sp -> if String.length sp > 0 then [sp] else [])

let p_quoted_string =
  let qcontent = satisfy is_qtext >|= String.make 1 <|> p_quoted_pair in
  p_cfws *> char '"' *> many (p_fws >>= fun sp -> qcontent >|= ( ^ ) sp)
  >|= (fun (_, l) -> String.concat "" l)
  >>= fun q_string -> p_fws >|= (fun sp -> q_string ^ sp) <* char '"'

let p_param_value = p_token <|> delay (f p_quoted_string)

let p_param =
  p_whitespace *> char ';' *> p_whitespace *> p_token
  >>= fun attribute ->
  char '=' *> p_param_value >|= fun value -> (attribute, value)

let p_restricted_name =
  let p_restricted_name_chars =
    satisfy (function
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
  satisfy is_alpha_digit
  >>= fun first_ch ->
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch ;
  many ~up_to:126 p_restricted_name_chars
  >|= fun (_, restricted_name) ->
  Buffer.add_string buf (implode restricted_name) ;
  Buffer.contents buf

let p_content_disposition =
  string "Content-Disposition"
  *> char ':'
  *> p_whitespace
  *> string "form-data"
  *> many p_param
  >|= fun (_, params) ->
  let params = List.to_seq params |> String_map.of_seq in
  Content_disposition params

let p_content_type parse_header_name =
  ( if parse_header_name then string "Content-Type:" *> char ':' *> unit
  else unit )
  *> p_whitespace
  *> p_restricted_name
  >>= fun ty ->
  char '/' *> p_restricted_name
  >>= fun subtype ->
  p_whitespace *> many p_param
  >|= fun (_, params) ->
  let parameters = params |> List.to_seq |> String_map.of_seq in
  Content_type {ty; subtype; parameters}

let p_header_boundary =
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
  let p_bchars =
    satisfy (function
        | '\x20' -> true
        | c when is_bcharnospace c -> true
        | _ -> false)
  in
  let is_dquote =
    satisfy (function
        | '"' -> true
        | _   -> false)
  in
  let boundary =
    many ~up_to:70 p_bchars
    >>= fun (_, bchars) ->
    let len = List.length bchars in
    if len > 0 then
      let last_char = List.nth bchars (len - 1) in
      if is_bcharnospace last_char then return (implode bchars)
      else fail "Invalid boundary value: invalid last char"
    else fail "Invalid boundary value: 0 length"
  in
  optional is_dquote *> boundary <* optional is_dquote <|> p_token

let p_multipart_formdata_header =
  let param =
    p_whitespace *> char ';' *> p_whitespace *> p_token
    >>= fun attribute ->
    ( char '='
    *> if attribute = "boundary" then p_header_boundary else p_param_value )
    >|= fun value -> (attribute, value)
  in
  ( optional (string "\r\n")
    *> optional (string "Content-Type:")
    *> p_whitespace
    *> string "multipart/form-data"
  <?> "Not multipart formdata header" )
  *> p_whitespace
  *> many param
  >|= fun (_, params) -> params |> List.to_seq |> String_map.of_seq

let body_part headers body =
  let name, content_type, filename, parameters =
    List.fold_left
      (fun (name, ct, filename, params) header ->
        match header with
        | Content_type ct ->
            let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
            ( name
            , content_type
            , filename
            , String_map.union (fun _key a _b -> Some a) params ct.parameters )
        | Content_disposition params2 ->
            let name = String_map.find_opt "name" params2 in
            let filename = String_map.find_opt "filename" params2 in
            ( name
            , ct
            , filename
            , String_map.union (fun _key a _b -> Some a) params params2 ))
      (None, None, None, String_map.empty)
      headers
  in
  match name with
  | None    -> fail "parameter 'name' not found"
  | Some nm ->
      let content_type = try Option.get content_type with _ -> "text/plain" in
      let parameters =
        String_map.remove "name" parameters
        |> fun parameters ->
        match filename with
        | Some _ -> String_map.remove "filename" parameters
        | None   -> parameters
      in
      ( match filename with
      | Some _ ->
          ( nm
          , File
              { File_part.filename
              ; content_type
              ; parameters
              ; body = Bytes.unsafe_of_string body } )
      | None   -> (nm, String body) )
      |> return

let add_part (name, bp) m =
  match String_map.find_opt name m with
  | Some l -> String_map.add name (bp :: l) m
  | None   -> String_map.add name [bp] m

let p_multipart_bodyparts boundary_value =
  let dash_boundary = "--" ^ boundary_value in
  let rec loop_body buf =
    optional line
    >>= function
    | Some ln ->
        if ln <> dash_boundary then (
          Buffer.add_string buf (ln ^ "\r\n") ;
          loop_body buf )
        else (Buffer.contents buf, Some ln) |> return
    | None    -> (Buffer.contents buf, None) |> return
  in
  let rec loop_parts parts = function
    | Some ln ->
        if ln = dash_boundary ^ "--" then return parts
        else if ln = dash_boundary then
          many
            ( string "\r\n" *> p_content_type true
            <|> delay (f p_content_disposition) )
          >>= fun (_, headers) ->
          loop_body (Buffer.create 0)
          >>= fun (body, ln) ->
          body_part headers body >>= fun bp -> loop_parts (bp :: parts) ln
        else optional line >>= loop_parts parts
    | None    -> return parts
  in
  optional line
  >>= loop_parts []
  >>= fun parts ->
  List.fold_left
    (fun m (name, bp) -> add_part (name, bp) m)
    String_map.empty
    parts
  |> return

let parse ~content_type_header ~body =
  let header_params =
    (parse content_type_header) p_multipart_formdata_header |> Result.get_ok
  in
  match String_map.find "boundary" header_params with
  | boundary_value      ->
      parse body (p_multipart_bodyparts boundary_value) |> Result.get_ok
  | exception Not_found ->
      raise @@ Http_multipart_formdata "Boundary paramater not found"
