type t

type error =
  private
  [> `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | Reparse.error ]

val form_field : t -> string

val filename : t -> string option

val content_type : t -> string

val is_file : t -> bool

val body : t -> bytes

val parse : header:string -> body:Reparse.input -> (t list, error) result

val sexp_of_t : t -> Sexplib0.Sexp.t

val pp : Format.formatter -> t -> unit
