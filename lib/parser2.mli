type error = [ `Msg of string ]

type (+'a, +'error) t

val of_string : string -> ('a, 'error) t -> ('a, 'error) result

(** {2 Basic Parsers} *)

val char : char -> (char, [> error ]) t

val char_if : (char -> bool) -> (char, [> error ]) t

val string : string -> (string, [> error ]) t

(** {3 Looping} *)

val skip_while : (char -> bool) -> (unit, [> error ]) t

val take_while : (char -> bool) -> (string, [> error ]) t

val take_while_n : int -> (char -> bool) -> (string, [> error ]) t

val many : ('a, [> error ]) t -> ('a list, [> error ]) t

val ok : 'a -> ('a, [> error ]) t
(** {2 Constructors} *)

val error : 'error -> ('a, 'error) t

(** {2 Monadic API} *)

val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t

val ( *> ) : (_, 'error) t -> ('a, 'error) t -> ('a, 'error) t
