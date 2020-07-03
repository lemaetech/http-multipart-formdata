type state =
  | Multipart_formdata
  | Multipart_body_part
  | Multipart_body_header_param

val lex_multipart_header : Lexing.lexbuf -> Parser.token

val lex_multipart_formdata : state ref -> Lexing.lexbuf -> Parser.token
