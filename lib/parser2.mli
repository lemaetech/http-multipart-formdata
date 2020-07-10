type error =
  [ `Msg of string
  | `Expected_char of int * char
  | `Expected_string of int * string
  | `Eof of int ]

type (+'a, +'error) t

val of_string : string -> ('a, 'error) t -> ('a, 'error) result

val char : char -> (char, [> error ]) t

val string : string -> (string, [> error ]) t

val skip_while : (char -> bool) -> (unit, [> error ]) t

val ok : 'a -> ('a, [> error ]) t

val error : 'error -> ('a, 'error) t

val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t

val ( *> ) : (_, 'error) t -> ('a, 'error) t -> ('a, 'error) t
