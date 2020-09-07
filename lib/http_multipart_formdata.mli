(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

(** Represents error while parsing http multipart formdata. *)
exception Http_multipart_formdata of string

module String_map : Map.S with type key = string

module File_part : sig
  (** Represents a File body part instance. *)
  type t

  val filename : t -> string option
  (** [filename t] returns [Some s] which represents the [filename] parameter of
      a file body part. Otherwise it returns [None] if the body part is just a
      string value. *)

  val content_type : t -> string
  (** [content_type t] returns content-type of [t] as specified in
      https://tools.ietf.org/html/rfc7578#section-4.4. *)

  val body : t -> bytes
  (** [body t] returns the body data of [t]. *)

  val find_parameter : string -> t -> string option

  (** {2 Pretty-printers}*)

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
end

(** HTTP multipart data. *)
type t = [`File of File_part.t | `String of string] list String_map.t

(** {2 Parsing} *)

val parse :
     content_type_header:string
  -> body:[`String of string | `Bigstring of Bigstringaf.t]
  -> t
(** [parse ~content_type_header ~body] parses HTTP [Content-Type]
    [content_type_header] to retrieve boundary value. It then uses that to parse
    [body] to return HTTP multpart form data items in a key/value map data
    structure.

    @raise Http_multipart_formdata *)

(** {2 Pretty-printers} *)

val sexp_of_t : t -> Sexplib0.Sexp.t
val pp : Format.formatter -> t -> unit
