open Std
open Sexplib.Std

type mode = Content_type | Content_type_param

type lb = mode Lexer.t

type result = (string, string) R.t [@@deriving sexp_of]

let rec lex_whitespace (lexer : lb) =
  if Char_code.is_whitespace lexer.ch then (
    Lexer.next lexer;
    lex_whitespace lexer )

(* From RFC https://tools.ietf.org/html/rfc6838#section-4.2
 type-name = restricted-name
 subtype-name = restricted-name

 restricted-name = restricted-name-first *126restricted-name-chars
 restricted-name-first  = ALPHA / DIGIT
 restricted-name-chars  = ALPHA / DIGIT / "!" / "#" /
                          "$" / "&" / "-" / "^" / "_"
 restricted-name-chars =/ "." ; Characters before first dot always
                              ; specify a facet name
 restricted-name-chars =/ "+" ; Characters after last plus always
                              ; specify a structured syntax suffix
*)
let lex_restricted_name (lexer : lb) =
  let rec lex count =
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
      lex (count + 1) )
  in

  Lexer.lex_start lexer;
  if Char_code.is_alpha lexer.ch || Char_code.is_digit lexer.ch then (
    Lexer.next lexer;
    lex 0;
    Lexer.lexeme lexer |> R.ok )
  else sprintf "Expected ALPHA|DIGIT but received %03d" lexer.ch |> R.error

(*
 content := "Content-Type" ":" type "/" subtype
            *(";" parameter)
            ; Matching of media type and subtype
            ; is ALWAYS case-insensitive.
*)
let lex_content_type lexer =
  let open R.O in
  lex_whitespace lexer;
  let* type_ = lex_restricted_name lexer in
  let* forward_slash = Lexer.accept Char_code.forward_slash lexer in
  let+ subtype = lex_restricted_name lexer in
  lex_whitespace lexer;
  type_ ^ forward_slash ^ subtype

(* token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials> *)
let lex_token lexer =
  Lexer.lex_start lexer;
  let rec lex () =
    if Char_code.is_token_char lexer.ch then (
      Lexer.next lexer;
      lex () )
    else Lexer.lexeme lexer |> Token.token |> R.ok
  in
  lex ()

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

let lex_quoted_string _lexer =
  (* quoted-pair = ('\' (VCHAR / WSP)) / obs-qp *)
  ()

let lex_header_param _t = R.ok Token.eof

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
