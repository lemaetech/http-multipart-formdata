open Std

type t

val current : t -> Char_token.t

val create : string -> t

val lex_start : t -> unit

val next : t -> unit

val peek : t -> Char_token.t

val peek2 : t -> Char_token.t

val lexeme : t -> string

val expect : Char_token.t -> t -> (unit, string) R.t

val accept : Char_token.t -> t -> (string, string) R.t
