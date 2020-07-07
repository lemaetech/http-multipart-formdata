open Std
open Sexplib.Std

type mode = Content_type | Content_type_param

type lexer = mode Lexer.t

type result = (string, string) R.t [@@deriving sexp_of]

let rec lex_whitespace (l : lexer) =
  if Lexer.current l |> Char_token.is_whitespace then (
    Lexer.next l;
    lex_whitespace l )

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
let parse_restricted_name (l : lexer) =
  let rec parse_restricted_char count =
    let ch = Lexer.current l in
    if
      (count < 126 && Char_token.is_alpha ch)
      || Char_token.is_digit ch
      || ch == Char_token.bang
      || ch == Char_token.hash
      || ch == Char_token.dollar
      || ch == Char_token.ampersand
      || ch == Char_token.minus
      || ch == Char_token.caret
      || ch == Char_token.underscore
      || ch == Char_token.dot
      || ch == Char_token.plus
    then (
      Lexer.next l;
      parse_restricted_char (count + 1) )
  in
  Lexer.lex_start l;
  let ch = Lexer.current l in
  if Char_token.is_alpha ch || Char_token.is_digit ch then (
    Lexer.next l;
    parse_restricted_char 0;
    Lexer.lexeme l |> R.ok )
  else
    asprintf "Expected ALPHA|DIGIT but received '%a'" Char_token.pp ch
    |> R.error

(* RFC - https://tools.ietf.org/html/rfc5322#section-3.2.2  
 FWS = ([*WSP CRLF] 1*WSP) /  obs-FWS   ; Folding white space
*)
let rec parse_fws (l : lexer) =
  lex_whitespace l;
  if
    Lexer.current l == Char_token.cr
    && Lexer.peek l == Char_token.lf
    && (Char_token.is_whitespace @@ Lexer.peek2 l)
  then (
    Lexer.(
      next l;
      next l;
      next l);
    parse_fws l )

let parse_quoted_pair (l : lexer) =
  if Lexer.current l == Char_token.back_slash then
    let lookahead = Lexer.peek l in
    if Char_token.is_vchar lookahead || Char_token.is_whitespace lookahead then (
      Lexer.lex_start l;
      Lexer.next l;
      Lexer.next l;
      Lexer.lexeme l |> R.ok )
    else
      sprintf "Invalid QUOTED_PAIR, VCHAR or WSP expected after '\'" |> R.error
  else R.ok ""

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
let parse_cfws (l : lexer) =
  let open R.O in
  let rec parse_comment () =
    let rec parse_ccontents () =
      parse_fws l;
      let ch = Lexer.current l in
      if Char_token.is_ctext ch then (
        Lexer.lex_start l;
        Lexer.next l;
        parse_ccontents () )
      else if ch == Char_token.back_slash then
        parse_quoted_pair l >>= fun _ -> parse_ccontents ()
      else if ch == Char_token.lparen then (
        Lexer.next l;
        parse_comment () >>= parse_ccontents )
      else R.ok ()
    in
    parse_ccontents () >>= fun () ->
    parse_fws l;
    Lexer.expect Char_token.rparen l
  in
  let rec parse_comments () =
    parse_fws l;
    if Lexer.current l == Char_token.lparen then (
      Lexer.next l;
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
let parse_quoted_string (l : lexer) =
  let open R.O in
  let rec parse_qcontents qcontent =
    parse_fws l;
    let ch = Lexer.current l in
    if Char_token.is_qtext ch then (
      Lexer.lex_start l;
      Lexer.next l;
      qcontent ^ Lexer.lexeme l |> parse_qcontents )
    else if ch == Char_token.back_slash then
      parse_quoted_pair l >>= fun quoted_pair ->
      parse_qcontents (qcontent ^ quoted_pair)
    else R.ok qcontent
  in

  parse_cfws l >>= fun () ->
  if Lexer.current l == Char_token.double_quote then (
    Lexer.next l;
    parse_qcontents "" >>= fun qcontent ->
    parse_fws l;
    Lexer.expect Char_token.double_quote l >>= fun () -> R.ok qcontent )
  else R.ok ""

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
let lex_content_type l =
  let open R.O in
  lex_whitespace l;
  let* type_ = parse_restricted_name l in
  let* forward_slash = Lexer.accept Char_token.forward_slash l in
  let+ subtype = parse_restricted_name l in
  lex_whitespace l;
  type_ ^ forward_slash ^ subtype

(* RFC - https://tools.ietf.org/html/rfc2045#section-5.1
   token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials> *)
let lex_token l =
  let is_tspecials ch =
    let open Char_token in
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
    let open Char_token in
    is_ascii ch && ch <> space && (not (is_control ch)) && not (is_tspecials ch)
  in
  Lexer.lex_start l;
  let rec lex () =
    if is_token_char (Lexer.current l) then (
      Lexer.next l;
      lex () )
    else Lexer.lexeme l |> Token.token |> R.ok
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
                                                                     "Expected ALPHA|DIGIT but received '!'") |}]

(* let%expect_test "lex_token" = *)
(*   [ "boundary ="; "bound\x7Fary"; "boundary"; "boundary    " ] *)
(*   |> List.map (Lexer.create Content_type) *)
(*   |> List.iter (lex_token >> pp_result); *)
(*   [%expect *)
(*     {| *)
(*     (Ok (Token boundary))(Ok (Token bound))(Ok (Token boundary))(Ok *)
(*                                                                  (Token boundary)) |}] *)
