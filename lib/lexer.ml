open Std

type mode =
  | Multipart_formdata
  | Multipart_body_part
  | Multipart_body_header_param

type t =
  { src : string
  ; src_len : int
  ; mutable ch : int (* current character *)
  ; mutable offset : int (* character offset *)
  ; mutable rd_offset : int (* reading offset (position after current character) *)
  }

module Char_code = struct
  let space = 0x20
  let htab = 0x09
  let null = 0x00
  let lparen = 0x28
  let rparen = 0x29
  let invalid = -1
  let less_than = 0x3C
  let greater_than = 0x3E
  let at = 0x40
  let comma = 0x2C
  let colon = 0x3A
  let semicolon = 0x3B
  let forward_slash = (* / *) 0x2F
  let back_slash = (* \ *) 0x5C
  let double_quote = 0x22
  let lbracket = 0x5B
  let rbracket = 0x5D
  let question = 0x3F
  let equal = 0x3D
  let is_vchar ch = ch >= 0x21 && ch <= 0x7E
  let is_whitespace ch = ch == space || ch == htab

  let is_tspecials ch =
    ch == lparen
    || ch == rparen
    || ch == less_than
    || ch == greater_than
    || ch == at
    || ch == comma
    || ch == semicolon
    || ch == colon
    || ch == back_slash
    || ch == double_quote
    || ch == forward_slash
    || ch == lbracket
    || ch == rbracket
    || ch == question
    || ch == equal


  let is_control ch = (ch >= 0x00 && ch <= 0x1F) || ch == 0x7F
  let is_ascii ch = ch >= 0x00 && ch <= 0x7F

  let is_token_char ch =
    is_ascii ch && ch <> space && (not (is_control ch)) && not (is_tspecials ch)
end

let next t =
  if t.rd_offset < t.src_len
  then (
    t.offset <- t.rd_offset;
    t.ch <- int_of_char t.src.[t.rd_offset];
    t.rd_offset <- t.rd_offset + 1)
  else (
    t.offset <- t.src_len;
    t.ch <- Char_code.invalid)


let peek t =
  if t.rd_offset < t.src_len then int_of_char t.src.[t.rd_offset] else Char_code.invalid


let create b =
  let t =
    { src = b; src_len = String.length b; ch = Char_code.null; offset = 0; rd_offset = 0 }
  in
  next t;
  t


let substring t start_offs =
  String.sub t.src start_offs (t.offset - start_offs) |> String.trim


let rec lex_whitespace t =
  if Char_code.is_whitespace t.ch
  then (
    next t;
    lex_whitespace t)
  else ()


(*
 field-content = field-vchar [ 1*( SP / HTAB ) field-vchar ]
 field-value = *( field-content / obs-fold )
 field-vchar = VCHAR / obs-text
*)
let lex_field_value t =
  let start_offs = t.offset in
  let rec lex () =
    if Char_code.is_vchar t.ch
    then (
      next t;
      lex ())
    else if Char_code.is_whitespace t.ch
    then (
      lex_whitespace t;
      if Char_code.is_vchar @@ peek t
      then (
        next t;
        lex ()))
  in
  lex ();
  substring t start_offs |> Token.header_field_value


(* token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials> *)
let lex_token t =
  let start_offs = t.offset in
  let rec lex () =
    if Char_code.is_token_char t.ch
    then (
      next t;
      lex ())
  in
  lex ();
  substring t start_offs |> Token.token


let lex_header_param _t = ()
let lex_formdata _t = ()

(* tokenizes HTTP 'Content-Type' header value, eg. multipart/form-data; boundary=7353230 *)
let lex_multipart_header_value _t = ()

(*--------------------------------------------*)
(*----------------- Unit Tests ---------------*)
(*--------------------------------------------*)

let pp = Token.pp Format.std_formatter

let%expect_test "lex_field_value" =
  [ "gzip, deflate ;"; "feed  "; "text/html ; " ]
  |> List.map create
  |> List.iter (lex_field_value >> pp);
  [%expect
    {|
    (Header_field_value "gzip, deflate")(Header_field_value feed)(Header_field_value
                                                                  text/html) |}]

let%expect_test "lex_token" =
  [ "boundary ="; "bound\x7Fary"; "boundary"; "boundary    " ]
  |> List.map create
  |> List.iter (lex_token >> pp);
  [%expect {| (Token boundary)(Token bound)(Token boundary)(Token boundary) |}]
