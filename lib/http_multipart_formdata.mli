(** {2 Types} *)

type t
(** Represents HTTP Multipart formadata. *)

type nonrec error =
  [ `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | `Invalid_multipart_body_header
  | `Name_parameter_not_found
  | Reparse.error ]
(** Represents error while parsing http multipart formdata. *)

module Body_part : sig
  type t
  (** Represents a body part instance. *)

  val name : t -> string
  (** [name t] returns value of [name] parameter. *)

  val filename : t -> string option

  val content_type : t -> string

  val is_file : t -> bool

  val body : t -> bytes

  val sexp_of_t : t -> Sexplib0.Sexp.t

  val pp : Format.formatter -> t -> unit
end

val parse :
  header:string ->
  body:[ `String of string | `Bigstring of Bigstringaf.t ] ->
  (t, error) result
(** [parse ~header ~body] parses [header] to retrieve boundary value and uses
    that to parse [body] to return [t]. *)

(** {2 Pretty-printers *)

val sexp_of_error : error -> Sexplib0.Sexp.t

val sexp_of_t : t -> Sexplib0.Sexp.t

val pp : Format.formatter -> t -> unit
