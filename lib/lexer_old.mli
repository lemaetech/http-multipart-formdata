type mode =
  | Multipart_formdata
  | Multipart_body_part
  | Multipart_body_header_param

val lex_multipart_header : Lexing.lexbuf -> Parser_old.token
val lex_multipart_formdata : mode ref -> Lexing.lexbuf -> Parser_old.token

(* val lex_concat : Lexing.lexbuf -> string *)
