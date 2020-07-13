type error = [ `Msg of string ]

type (+'a, +'error) t

val advance : int -> (unit, [> error ]) t

val of_string : string -> ('a, ([> error ] as 'b)) t -> ('a, 'b) result

(** {2 Basic Parsers} *)

val char : char -> (char, [> error ]) t

val satisfy : (char -> bool) -> (char, [> error ]) t

val peek_char : (char option, [> error ]) t
(** [peek_char t] returns a character at the current position in the parser.
    Always suceeds and returns [None] if EOF is reached. *)

val peek_char_fail : (char, [> error ]) t

val peek_string : int -> (string option, [> error ]) t

val string : string -> (string, [> error ]) t

val skip_while : (char -> bool) -> (unit, [> error ]) t
(** [skip_while f] keeps accepting [c] if [f c] is [true]. [c] is discarded.
    Always succeeds. *)

val count_skip_while : (char -> bool) -> (int, [> error ]) t
(** [count_skip_while f] loops through input accepting/advancing [c] only if
    [f c] is true. Returns the count of times the [f c] was true. *)

val count_skip_while_string : int -> (string -> bool) -> (int, [> error ]) t
(** [count_skip_while_string n f] loops through input accepting/advancing [s]
    only if [f s] is true. [String.length s = n] Returns the count of times the
    [f s] was true. *)

val take_while : (char -> bool) -> (string, [> error ]) t

val take_while_n : int -> (char -> bool) -> (string, [> error ]) t

(** {2 Constructors} *)

val ok : 'a -> ('a, [> error ]) t

val fail : 'error -> ('a, 'error) t

(** {2 Alternatives} *)

val ( <|> ) : ('a, 'error) t -> ('a, 'error) t -> ('a, 'error) t

(** {2 Monadic API} *)

val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t

val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t

val ( *> ) : (_, 'error) t -> ('a, 'error) t -> ('a, 'error) t

(** {2 Combinators} *)

val many : ('a, [> error ]) t -> ('a list, [> error ]) t

val count_skip_many : ('a, [> error ]) t -> (int, [> error ]) t
(** [count_skip_many p] runs [p] zeor or more times *)
