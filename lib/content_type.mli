type t

val parse_content_type : Parser.t -> (t, string) Std.R.t

val validate_boundary_value : string -> (unit, string) Std.R.t
