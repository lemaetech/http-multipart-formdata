(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

type reader

and read_result =
  [ `End
  | `Header of part_header
  | `Body of Cstruct.t
  | `Body_end
  | `Awaiting_input of [`Cstruct of Cstruct.t | `Eof] -> read_result
  | `Error of string ]

and bigstring = Bigstringaf.t

(** Represents a parsed multipart part header data. *)
and part_header

(** Represents the multipart boundary value. *)
and boundary

(** {2 Mulipart Boundary parser} *)

val boundary : content_type:string -> (boundary, string) result
(** [parse_boundary ~content_type] parses [content_type] to extract [boundary]
    value.[content_type] is the HTTP request [Content-Type] header value. *)

(** {2 Multipart Reader} *)

val reader : ?read_body_len:int -> boundary -> reader
(** [reader ?read_body_len boundary input] creates reader. The default value for
    [read_body_len] is 1KB. *)

val read_part : reader -> read_result
(** [read_part ?read_body_len ~boundary reader] reads a http multipart body and
    returns a [read_result]. *)

(** {2 Part header} *)

val name : part_header -> string
(** [name t] returns the form field name *)

val content_type : part_header -> string
(** [content_type t] returns the part content-type. *)

val filename : part_header -> string option
(** [filename t] returns the uploaded filename is the multipart is a file *)

val find : string -> part_header -> string option
(** [param_value name t] returns the multipart parameter value with name [name]. *)

(** {2 Pretty Printers} *)

val pp_part_header : Format.formatter -> part_header -> unit
val pp_read_result : Format.formatter -> read_result -> unit
val pp_boundary : Format.formatter -> boundary -> unit
