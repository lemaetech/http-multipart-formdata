type t

val parse_content_type : Parser.t -> (t, string) Std.R.t

val validate_boundary_value : string -> (unit, string) Std.R.t

val ty : t -> string

val subtype : t -> string

val parameters : t -> (string * string) Seq.t

val find_parameter : name:string -> t -> string option
