(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

exception Http_multipart_formdata of string
(** Represents error while parsing http multipart formdata. *)

module String_map : Map.S with type key = string

module File_part : sig
  type t
  (** Represents a File body part instance. *)

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

type t = [ `File of File_part.t | `String of string ] list String_map.t
(** HTTP multipart data. *)

(** {2 Parsing} *)

val parse :
  header:string -> body:[ `String of string | `Bigstring of Bigstringaf.t ] -> t
(** [parse ~header ~body] parses [header] to retrieve boundary value and uses
    that to parse [body] to return [t].

    @raise Http_multipart_formdata *)

(** {2 Pretty-printers} *)

val sexp_of_t : t -> Sexplib0.Sexp.t

val pp : Format.formatter -> t -> unit
