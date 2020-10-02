(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
open Sexplib0
open Sexplib0.Sexp_conv
module P = Reparse.Parser.String_parser
open P.Infix

exception Multipart_formdata of string

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

  let sexp_of_t {filename; content_type; parameters; body} =
    Sexp.List
      [ Sexp.List [Sexp.Atom "filename"; sexp_of_option sexp_of_string filename]
      ; Sexp.List [Sexp.Atom "content_type"; sexp_of_string content_type]
      ; Sexp.List
          [ Sexp.Atom "parameters"
          ; String_map.sexp_of_t sexp_of_string parameters ]
      ; Sexp.List [Sexp.Atom "body"; sexp_of_bytes body] ]

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

let rec sexp_of_t v = String_map.sexp_of_t (sexp_of_list sexp_of_part) v

and sexp_of_part = function
  | File f   -> Sexp.List [Sexp.Atom "File"; File_part.sexp_of_t f]
  | String s -> Sexp.List [Sexp.Atom "String"; sexp_of_string s]

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
  | _                -> false

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
  is_ascii_char c
  && (not (is_space c))
  && (not (is_control c))
  && not (is_tspecial c)

let implode l = List.to_seq l |> String.of_seq

let token =
  P.take ~at_least:1 (P.satisfy is_token_char)
  <?> "[token]"
  >|= fun (_, chars) -> implode chars

(* https://tools.ietf.org/html/rfc5322#section-3.2.1
   quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp *)
let quoted_pair = String.make 1 <$> P.char '\\' *> (P.whitespace <|> P.vchar)

(* 
  Folding whitespace and comments - https://tools.ietf.org/html/rfc5322#section-3.2.2 

  let r = P.parse "    \r\n    " fws
  r = " "
 *)
let fws =
  P.skip P.whitespace
  >>= fun ws_count1 ->
  P.skip (P.string "\r\n" *> P.skip ~at_least:1 P.whitespace)
  >|= fun lws_count -> if ws_count1 + lws_count > 0 then " " else ""

(*
  let r = P.parse "(    asdfasdfasdfasd(aaa) \\(cccc\\) (bbb(ddd)))" comments;;
  r = " asdfasdfasdfasd;aaa (cccc) ;bbb;ddd";;
 *)
let comment =
  let ctext = P.satisfy is_ctext >|= String.make 1 in
  let rec loop_comments () =
    let ccontent =
      P.take
        (P.map2
           (fun sp content -> sp ^ content)
           fws
           (P.any [ctext; quoted_pair; loop_comments () >|= ( ^ ) ";"]))
      >|= fun (_, s) -> String.concat "" s
    in
    P.char '(' *> P.map2 (fun comment_txt sp -> comment_txt ^ sp) ccontent fws
    <* P.char ')'
  in
  loop_comments ()

(*
  let r = P.parse "  (    asdfasdfasdfasd(aaa) \\(cccc\\) (bbb(ddd)))   " cfws;;
  r = " "
 *)
let cfws =
  let one_or_more_comments =
    P.take
      ~at_least:1
      (P.map2 (fun sp comment_txt -> sp ^ comment_txt) fws comment)
    *> fws
  in
  one_or_more_comments <|> fws

(* Test cases : 
  let r = P.parse "\"    \r\n            hello   \r\n          \"" quoted_string;;
  r = " hello ";;

  P.parse "(comment1)\"hello\"(comment2)" quoted_string;;
  r = "hello";;
*)
let quoted_string =
  let qtext = String.make 1 <$> P.satisfy is_qtext in
  let qcontent =
    (fun (_, l) -> String.concat "" l)
    <$> P.take
          (P.map2
             (fun sp qcontent' -> sp ^ qcontent')
             fws
             (qtext <|> quoted_pair))
  in
  cfws *> P.dquote *> P.map2 (fun qcontent' sp -> qcontent' ^ sp) qcontent fws
  <* P.dquote
  <* cfws

(*
  let r = P.parse "asdfasdf" p_param_value;;
  r = "asdfasdf";;

  P.parse "\"hello\"" p_param_value;;
  r = "hello"
 *)
let param_value = token <|> quoted_string

(*
  let r = P.parse "; field1=value1;" p_param;;       
  r = ("field1", "value1");;

  let r = P.parse "; field1=\"value1\";" p_param;;
  r = ("field1", "value1");;
 *)
let param =
  let name =
    P.skip P.whitespace *> P.char ';' *> P.skip P.whitespace *> token
  in
  let value = P.char '=' *> param_value in
  P.map2 (fun name value -> (name, value)) name value

let p_restricted_name =
  let p_restricted_name_chars =
    P.satisfy (function
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
  P.satisfy is_alpha_digit
  >>= fun first_ch ->
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch ;
  P.take ~up_to:126 p_restricted_name_chars
  >|= fun (_, restricted_name) ->
  Buffer.add_string buf (implode restricted_name) ;
  Buffer.contents buf

let content_disposition =
  P.delay
    ( lazy
      ( P.string "Content-Disposition:"
        *> P.skip P.whitespace
        *> P.string "form-data"
        *> P.take param
      >|= fun (_, params) ->
      let params = List.to_seq params |> String_map.of_seq in
      Content_disposition params ) )

let content_type parse_header_name =
  P.delay
    ( lazy
      ( ( if parse_header_name then P.string "Content-Type:" *> P.unit
        else P.unit )
        *> P.skip P.whitespace
        *> p_restricted_name
      >>= fun ty ->
      P.char '/' *> p_restricted_name
      >>= fun subtype ->
      P.take param
      >|= fun (_, params) ->
      let parameters = params |> List.to_seq |> String_map.of_seq in
      Content_type {ty; subtype; parameters} ) )

let header_boundary =
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
    P.satisfy (function
        | '\x20' -> true
        | c when is_bcharnospace c -> true
        | _ -> false)
  in
  let boundary =
    P.take ~up_to:70 bchars
    >>= fun (_, bchars) ->
    let len = List.length bchars in
    if len > 0 then
      let last_char = List.nth bchars (len - 1) in
      if is_bcharnospace last_char then P.return (implode bchars)
      else P.fail "Invalid boundary value: invalid last char"
    else P.fail "Invalid boundary value: 0 length"
  in
  P.optional P.dquote *> boundary <* P.optional P.dquote <|> token

let multipart_formdata_header =
  let param =
    P.skip P.whitespace *> P.char ';' *> P.skip P.whitespace *> token
    >>= fun attribute ->
    ( P.char '='
    *> if attribute = "boundary" then header_boundary else param_value )
    >|= fun value -> (attribute, value)
  in
  ( P.optional P.crlf
    *> P.optional (P.string "Content-Type:")
    *> P.skip P.whitespace
    *> P.string "multipart/form-data"
  <?> "Not multipart formdata header" )
  *> P.skip P.whitespace
  *> P.take param
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
  | None    -> P.fail "parameter 'name' not found"
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
      |> P.return

let add_part (name, bp) m =
  match String_map.find_opt name m with
  | Some l -> String_map.add name (bp :: l) m
  | None   -> String_map.add name [bp] m

let multipart_bodyparts boundary_value =
  let dash_boundary = "--" ^ boundary_value in
  let end_boundary = dash_boundary ^ "--" in
  let line = P.line `CRLF in
  let rec loop_body buf =
    line
    >>= fun ln ->
    if ln = dash_boundary then P.return (Buffer.contents buf, true)
    else if ln = end_boundary then P.return (Buffer.contents buf, false)
    else (
      Buffer.add_string buf ln ;
      Buffer.add_string buf "\r\n" ;
      loop_body buf )
  in
  let rec loop_parts parts =
    P.take
      ~at_least:1
      ~sep_by:P.crlf
      (P.any [content_disposition; content_type true])
    >|= snd
    >>= fun part_headers ->
    loop_body (Buffer.create 0)
    >>= fun (body, continue) ->
    body_part part_headers body
    >>= fun bp ->
    if continue then loop_parts (bp :: parts) else P.return (bp :: parts)
  in
  P.all_unit [P.crlf; P.string dash_boundary; P.crlf] *> loop_parts []
  >|= fun parts ->
  List.fold_left
    (fun m (name, bp) -> add_part (name, bp) m)
    String_map.empty
    parts

let parse ~content_type_header ~body =
  let header_params =
    P.parse
      (Reparse.Source.String.create content_type_header)
      multipart_formdata_header
  in
  match String_map.find "boundary" header_params with
  | boundary_value      ->
      P.parse
        (Reparse.Source.String.create body)
        (multipart_bodyparts boundary_value)
  | exception Not_found ->
      raise @@ Multipart_formdata "Boundary paramater not found"
