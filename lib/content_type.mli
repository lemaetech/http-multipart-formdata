type t

val ty : t -> string

val subtype : t -> string

val parameters : t -> (string * string) Seq.t

val parse : string -> (t, string) Result.t

val validate_boundary_value : string -> (unit, string) Result.t

val find_parameter : name:string -> t -> string option

val sexp_of_t : t -> Sexplib.Sexp.t

val pp : Format.formatter -> t -> unit
