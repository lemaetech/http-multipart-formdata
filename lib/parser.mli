open Std

type 'mode t

val current : 'mode t -> Char_token.t

val create : 'mode -> string -> 'mode t

val lex_start : 'mode t -> unit

val next : 'mode t -> unit

val peek : 'mode t -> Char_token.t

val peek2 : 'mode t -> Char_token.t

val lexeme : 'mode t -> string

val expect : Char_token.t -> 'mode t -> (unit, string) R.t

val accept : Char_token.t -> 'mode t -> (string, string) R.t
