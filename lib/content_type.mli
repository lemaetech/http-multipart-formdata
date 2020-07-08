type t

val parse_content_type : Parser.t -> (t, string) Result.t

val validate_boundary_value : string -> (unit, string) Result.t

val ty : t -> string

val subtype : t -> string

val parameters : t -> (string * string) Seq.t

val find_parameter : name:string -> t -> string option

val sexp_of_t : t -> Sexplib.Sexp.t
