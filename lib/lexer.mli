type error = private [> `Invalid_boundary_value ]

val sexp_of_error : error -> Sexplib0.Sexp.t

val sexp_of_result : (string, error) result -> Sexplib0.Sexp.t

val lex_boundary : Lexing.lexbuf -> (string, error) result
