(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Parsing boundary value} *)

(** Represents the multipart boundary value. *)
type boundary = string

val parse_boundary : content_type:string -> (boundary, string) result
(** [parse_boundary ~content_type] parses [content_type] to extract [boundary]
    value.[content_type] is the HTTP request [Content-Type] header value. *)

(** {2 Parsing multi-parts}

    [parse_parts_* ?part_stream_chunk_size ~boundary ~on_part http_post_body]
    functions with various input types.

    - [part_stream_chunk_size] is the maximum number of bytes each chunk holds
      at any time. The default value is [1048576] or [1MB].

    - [boundary] is part boundary value. Use [parse_boundary] to parse boundary
      value from [Content-type] header value.

    - [on_part] is the part handling function

    - [body] is the raw HTTP POST request body content stream. *)

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

val parse_parts_stream :
     ?part_stream_chunk_size:int
  -> boundary:boundary
  -> on_part:(Part_header.t -> char Lwt_stream.t -> unit Lwt.t)
  -> char Lwt_stream.t
  -> (unit, string) result Lwt.t

val parse_parts_fd :
     ?part_stream_chunk_size:int
  -> boundary:boundary
  -> on_part:(Part_header.t -> char Lwt_stream.t -> unit Lwt.t)
  -> Lwt_unix.file_descr
  -> (unit, string) result Lwt.t

val parse_parts_channel :
     ?part_stream_chunk_size:int
  -> boundary:boundary
  -> on_part:(Part_header.t -> char Lwt_stream.t -> unit Lwt.t)
  -> Lwt_io.input_channel
  -> (unit, string) result Lwt.t
