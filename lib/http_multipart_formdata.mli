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

(** [http_body] represents various HTTP POST body stream. *)
type http_body =
  [ `Stream of char Lwt_stream.t
  | `Fd of Lwt_unix.file_descr
  | `Channel of Lwt_io.input_channel
  ]

(** Represents a parsed multipart part header data. *)
type part

(** [parse_boundary ~content_type] parses [content_type] to extract [boundary]
    value.[content_type] is the HTTP request [Content-Type] header value. *)
val parse_boundary : content_type:string -> (boundary, string) result

(** [name t] returns the form field name *)
val name : part -> string

(** [content_type t] returns the part content-type. *)
val content_type : part -> string

(** [filename t] returns the uploaded filename is the multipart is a file *)
val filename : part -> string option

(** [param_value name t] returns the multipart parameter value with name [name]. *)
val param_value : string -> part -> string option

val compare_part : part -> part -> int

val equal_part : part -> part -> bool

val pp_part : Format.formatter -> part -> unit

(** [parse_parts ?part_stream_chunk_size ~boundary ~on_part http_body] is a push
    based http multipart/formdata parser.

    - [part_stream_chunk_size] is the maximum number of bytes each chunk holds
      at any time. The default value is [1048576] or [1MB].

    - [boundary] is part boundary value. Use {!parse_boundary} to parse boundary
      value from [Content-type] header value.

    - [on_part] is the part handling function

    - [http_body] is the raw HTTP POST request body content stream. *)
val parse_parts :
     ?part_stream_chunk_size:int
  -> boundary:boundary
  -> on_part:(part -> part_body_stream:char Lwt_stream.t -> unit Lwt.t)
  -> http_body
  -> (unit, string) result Lwt.t
