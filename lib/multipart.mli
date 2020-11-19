(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

exception Multipart of string
(** Represents error while parsing http multipart formdata. *)

module Map : Map.S with type key = string

module Part : sig
  type t =
    { body: bytes
    ; name: string
    ; content_type: string
    ; filename: string option
    ; parameters: string Map.t }

  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val equal : t -> t -> bool
end

type t = parts Map.t
(** HTTP multipart data. *)

and parts = Part.t list

val pp_parts : Format.formatter -> parts -> unit [@@ocaml.toplevel_printer]
val equal_parts : parts -> parts -> bool
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
val equal : t -> t -> bool

(** {2 Parsing} *)

val parse : content_type_header:string -> body:string -> t
(** [parse ~content_type_header ~body] parses HTTP [Content-Type]
    [content_type_header] to retrieve boundary value. It then uses that to parse
    [body] to return HTTP multpart form data items in a key/value map data
    structure.

    @raise Multipart *)
