type t

val of_char : char -> t

val pp : Format.formatter -> t -> unit

val space : t

val cr : t

val lf : t

val htab : t

val null : t

val lparen : t

val rparen : t

val eof : t

val less_than : t

val greater_than : t

val at : t

val comma : t

val colon : t

val semicolon : t

val forward_slash : t

val back_slash : t

val double_quote : t

val single_quote : t

val lbracket : t

val rbracket : t

val question : t

val equal : t

val bang : t

val hash : t

val dollar : t

val ampersand : t

val minus : t

val caret : t

val underscore : t

val dot : t

val plus : t

val is_alpha : t -> bool

val is_digit : t -> bool

val is_vchar : t -> bool

val is_whitespace : t -> bool

val is_control : t -> bool

val is_ascii : t -> bool

val is_ctext : t -> bool

val is_qtext : t -> bool
