(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

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
  (** [filename t] returns [Some s] which represents the [filename] parameter of
      a file body part. Otherwise it returns [None] if the body part is just a
      string value. *)

  val content_type : t -> string
  (** [content_type t] returns content-type of [t] as specified in
      https://tools.ietf.org/html/rfc7578#section-4.4. *)

  val is_file : t -> bool
  (** [is_file t] returns [true] if [t] is a file body part. *)

  val body : t -> bytes
  (** [body t] returns the body data of [t]. *)

  val sexp_of_t : t -> Sexplib0.Sexp.t

  val pp : Format.formatter -> t -> unit
end

(** {2 Parsing} *)

val parse :
  header:string ->
  body:[ `String of string | `Bigstring of Bigstringaf.t ] ->
  (t, error) result
(** [parse ~header ~body] parses [header] to retrieve boundary value and uses
    that to parse [body] to return [t]. *)

(** {2 Query functions} *)

val find : string -> t -> Body_part.t list
(** [find nm t] returns a list of [Body_part.t] associated with name [nm]. It
    returns an empty list if [nm] is not found in [t]. *)

val body_parts : t -> Body_part.t list
(** [body_parts t] returns all parsed body parts in [t]. *)

(** {2 Pretty-printers *)

val sexp_of_error : error -> Sexplib0.Sexp.t

val sexp_of_t : t -> Sexplib0.Sexp.t

val pp : Format.formatter -> t -> unit
