(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
type input =
  [ `Stream of char Lwt_stream.t
  | `Fd of Lwt_unix.file_descr
  | `Channel of Lwt_io.input_channel ]

type boundary = string
(** Represents the multipart boundary value. *)

type part_header
(** Represents a parsed multipart part header data. *)

val parse_boundary : content_type:string -> (boundary, string) result
(** [parse_boundary ~content_type] parses [content_type] to extract [boundary]
    value.[content_type] is the HTTP request [Content-Type] header value. *)

val name : part_header -> string
(** [name t] returns the form field name *)

val content_type : part_header -> string
(** [content_type t] returns the part content-type. *)

val filename : part_header -> string option
(** [filename t] returns the uploaded filename is the multipart is a file *)

val param_value : string -> part_header -> string option
(** [param_value name t] returns the multipart parameter value with name [name]. *)

val compare_part_header : part_header -> part_header -> int

val equal_part_header : part_header -> part_header -> bool

val pp_part_header : Format.formatter -> part_header -> unit

val parse_parts :
  ?part_stream_chunk_size:int ->
  boundary:boundary ->
  on_part:(part_header -> part_body_stream:char Lwt_stream.t -> unit Lwt.t) ->
  input ->
  (unit, string) result Lwt.t
(** [parse_parts ?part_stream_chunk_size ~boundary ~on_part http_body] is a push
    based http multipart/formdata parser.

    - [part_stream_chunk_size] is the maximum number of bytes each chunk holds
      at any time. The default value is [1048576] or [1MB].

    - [boundary] is part boundary value. Use {!parse_boundary} to parse boundary
      value from [Content-type] header value.

    - [on_part] is the part handling function

    - [http_body] is the raw HTTP POST request body content stream. *)

module type MULTIPART_PARSER = sig
  type input

  type 'a t

  type reader

  and read_result =
    [ `End
    | `Header of part_header
    | `Body of bigstring * int
    | `Error of string ]

  and bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val reader : ?read_body_len:int -> boundary -> input -> reader
  (** [reader ?read_body_len boundary input] creates reader. The default value
      for [read_body_len] is 1KB. *)

  val parse_parts :
    ?part_stream_chunk_size:int ->
    boundary:boundary ->
    on_part:(part_header -> part_body_stream:char Lwt_stream.t -> unit Lwt.t) ->
    input ->
    (unit * int, string) result Lwt.t

  val parse_part : reader -> read_result Lwt.t
  (** [parse_part ?read_body_len ~boundary reader] parse http multipart body and
      returns a [read_result].

      [read_body_len] determines the size of the multipart body to read in
      bytes. By default 1KB. *)
end

module Make (P : Reparse.PARSER with type 'a promise = 'a Lwt.t) :
  MULTIPART_PARSER with type input = P.input with type 'a t = 'a P.t
