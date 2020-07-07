open Std
open Sexplib.Std

type mode = Content_type | Content_type_param

type l = mode Lexer.t

type result = (string, string) R.t [@@deriving sexp_of]

let rec lex_whitespace (lexer : l) =
  if Char_code.is_whitespace lexer.ch then (
    Lexer.next lexer;
    lex_whitespace lexer )

(* From RFC https://tools.ietf.org/html/rfc6838#section-4.2
 restricted-name = restricted-name-first *126restricted-name-chars
 restricted-name-first  = ALPHA / DIGIT
 restricted-name-chars  = ALPHA / DIGIT / "!" / "#" /
                          "$" / "&" / "-" / "^" / "_"
 restricted-name-chars =/ "." ; Characters before first dot always
                              ; specify a facet name
 restricted-name-chars =/ "+" ; Characters after last plus always
                              ; specify a structured syntax suffix
*)
let parse_restricted_name (lexer : l) =
  let rec parse_restricted_char count =
    if
      (count < 126 && Char_code.is_alpha lexer.ch)
      || Char_code.is_digit lexer.ch
      || lexer.ch == Char_code.bang
      || lexer.ch == Char_code.hash
      || lexer.ch == Char_code.dollar
      || lexer.ch == Char_code.ampersand
      || lexer.ch == Char_code.minus
      || lexer.ch == Char_code.caret
      || lexer.ch == Char_code.underscore
      || lexer.ch == Char_code.dot
      || lexer.ch == Char_code.plus
    then (
      Lexer.next lexer;
      parse_restricted_char (count + 1) )
  in
  Lexer.lex_start lexer;
  if Char_code.is_alpha lexer.ch || Char_code.is_digit lexer.ch then (
    Lexer.next lexer;
    parse_restricted_char 0;
    Lexer.lexeme lexer |> R.ok )
  else sprintf "Expected ALPHA|DIGIT but received %03d" lexer.ch |> R.error

(* RFC - https://tools.ietf.org/html/rfc5322#section-3.2.2  
 FWS = ([*WSP CRLF] 1*WSP) /  obs-FWS   ; Folding white space
*)
let rec parse_fws (lexer : l) =
  lex_whitespace lexer;
  if
    lexer.ch == Char_code.cr
    && Lexer.peek lexer == Char_code.lf
    && (Char_code.is_whitespace @@ Lexer.peek2 lexer)
  then (
    Lexer.(
      next lexer;
      next lexer;
      next lexer);
    parse_fws lexer )

(* RFC -https://tools.ietf.org/html/rfc5322#section-3.2.1
 quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp

 RFC - https://tools.ietf.org/html/rfc5322#section-3.2.2
 ctext           =   %d33-39 /          ; Printable US-ASCII
                     %d42-91 /          ;  characters not including
                     %d93-126 /         ;  '(', ')', or '\'
                     obs-ctext

 ccontent        =   ctext / quoted-pair / comment

 comment         =   '(' *([FWS] ccontent) [FWS] ')'

 CFWS            =   (1*([FWS] comment) [FWS]) / FWS   
*)
let parse_cfws (lexer : l) =
  let open R.O in
  let rec parse_comment () =
    let rec parse_ccontents () =
      parse_fws lexer;
      if Char_code.is_ctext lexer.ch then (
        Lexer.lex_start lexer;
        Lexer.next lexer;
        parse_ccontents () )
      else if lexer.ch == Char_code.back_slash then
        let lookahead = Lexer.peek lexer in
        if Char_code.is_vchar lookahead || Char_code.is_whitespace lookahead
        then (
          Lexer.next lexer;
          Lexer.next lexer;
          parse_ccontents () )
        else
          sprintf "Invalid QUOTED_PAIR, VCHAR or WSP expected after '\'"
          |> R.error
      else if lexer.ch == Char_code.lparen then (
        Lexer.next lexer;
        parse_comment () >>= parse_ccontents )
      else R.ok ()
    in
    parse_ccontents () >>= fun () ->
    parse_fws lexer;
    Lexer.expect Char_code.rparen lexer
  in
  let rec parse_comments () =
    parse_fws lexer;
    if lexer.ch == Char_code.lparen then (
      Lexer.next lexer;
      parse_comment () >>= parse_comments )
    else R.ok ()
  in
  parse_comments ()

(* RFC - https://tools.ietf.org/html/rfc5322#section-3.2.4
 qtext           = %d33 /             ; Printable US-ASCII
                   %d35-91 /          ;  characters not including
                   %d93-126 /         ;  '\' or the quote character
                   obs-qtext

 qcontent        = qtext / quoted-pair

 quoted-string   = [CFWS]
                   DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                   [CFWS]
*)
let parse_quoted_string _lexer = ()

(* 

*)
let parse_header_param _t = R.ok Token.eof

(* RFC - https://tools.ietf.org/html/rfc2045#section-5.1
 content := "Content-Type" ":" type "/" subtype
            *(";" parameter)
            ; Matching of media type and subtype
            ; is ALWAYS case-insensitive.

 RFC - https://tools.ietf.org/html/rfc6838#section-4.2
 type-name = restricted-name
 subtype-name = restricted-name
*)
let lex_content_type lexer =
  let open R.O in
  lex_whitespace lexer;
  let* type_ = parse_restricted_name lexer in
  let* forward_slash = Lexer.accept Char_code.forward_slash lexer in
  let+ subtype = parse_restricted_name lexer in
  lex_whitespace lexer;
  type_ ^ forward_slash ^ subtype

(* RFC - https://tools.ietf.org/html/rfc2045#section-5.1
   token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials> *)
let lex_token lexer =
  let is_tspecials ch =
    let open Char_code in
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
  in
  let is_token_char ch =
    let open Char_code in
    is_ascii ch && ch <> space && (not (is_control ch)) && not (is_tspecials ch)
  in
  Lexer.lex_start lexer;
  let rec lex () =
    if is_token_char lexer.ch then (
      Lexer.next lexer;
      lex () )
    else Lexer.lexeme lexer |> Token.token |> R.ok
  in
  lex ()

(*--------------------------------------------*)
(*----------------- Unit Tests ---------------*)
(*--------------------------------------------*)

let pp_result r = Sexplib.Sexp.pp_hum Format.std_formatter (sexp_of_result r)

let%expect_test "lex_content_type" =
  [
    "multipart/form-data ";
    "   text/plain  ";
    "text/html ; ";
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
    "application/vnd.adobe.air-application-installer-package+zip";
    " !!";
  ]
  |> List.map (Lexer.create Content_type)
  |> List.iter (lex_content_type >> pp_result);
  [%expect
    {|
    (Ok multipart/form-data)(Ok text/plain)(Ok text/html)(Ok
                                                          application/vnd.openxmlformats-officedocument.wordprocessingml.document)
    (Ok application/vnd.adobe.air-application-installer-package+zip)(Error
                                                                     "Expected ALPHA|DIGIT but received 033") |}]

(* let%expect_test "lex_token" = *)
(*   [ "boundary ="; "bound\x7Fary"; "boundary"; "boundary    " ] *)
(*   |> List.map (Lexer.create Content_type) *)
(*   |> List.iter (lex_token >> pp_result); *)
(*   [%expect *)
(*     {| *)
(*     (Ok (Token boundary))(Ok (Token bound))(Ok (Token boundary))(Ok *)
(*                                                                  (Token boundary)) |}] *)
