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

let substring t start_offs = String.sub t.src start_offs (t.offset - start_offs)

let rec lex_whitespace t =
  if Char_code.is_whitespace t.ch
  then (
    next t;
    lex_whitespace t)

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

(* 
 https://tools.ietf.org/html/rfc5322#section-3

 qtext           = %d33 /             ; Printable US-ASCII
                   %d35-91 /          ;  characters not including
                   %d93-126 /         ;  '\' or the quote character
                   obs-qtext

 qcontent        = qtext / quoted-pair

 quoted-string   = [CFWS]
                   DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                   [CFWS]
*)

(*  
 FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                           ; Folding white space
*)
(* let lex_fws t = *)
(*   let rec lex () = *)
(*    lex_whitespace t; *)
(*      if peek t == *)

let lex_quoted_string _t =
  (* quoted-pair = ('\' (VCHAR / WSP)) / obs-qp *)
  ()

let lex_header_param _t = Token.eof

(* tokenizes HTTP 'Content-Type' header value, eg. multipart/form-data; boundary=7353230 *)
let lex_multipart_header_value t =
  if Char_code.is_vchar t.ch
  then lex_field_value t
  else if t.ch == Char_code.semicolon
  then (
    next t;
    lex_header_param t)
  else Token.eof

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
