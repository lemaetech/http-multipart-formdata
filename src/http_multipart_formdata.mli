(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** Represents a parsed multipart part header data. *)
module Part_header : sig
  type t

  val name : t -> string
  val content_type : t -> string
  val filename : t -> string option
  val param_value : string -> t -> string option
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(** Represents the multipart boundary value. *)
type boundary = string

val parse_boundary : content_type:string -> (boundary, string) result Lwt.t
(** [parse_boundary ~content_type] parses [content_type] to extract [boundary]
    value.[content_type] is the HTTP request [Content-Type] header value. *)

val parse_parts :
     ?part_stream_chunk_size:int
  -> boundary:boundary
  -> on_part:(Part_header.t -> char Lwt_stream.t -> unit Lwt.t)
  -> char Lwt_stream.t
  -> (unit, string) result Lwt.t
(** [parse ~part_stream_chunk_size ~boundary ~on_part body] parses [body] and
    streams [part_header] and [part_body_data] to [on_part].

    [boundary] is part boundary value

    [body] is the raw HTTP POST request body content stream. *)
