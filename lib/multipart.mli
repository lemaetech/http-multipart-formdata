(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

module Map : Map.S with type key = string
(** An ocaml [Stdlib] Map with [string] as key. *)

(** Represents a parsed multipart part. A part corresponds to a submitted form
    field data in a HTTP request. *)
module Part : sig
  type t =
    { body: bytes  (** Body content *)
    ; name: string  (** Name of the part - form field name *)
    ; content_type: string
          (** HTTP content type of the part [body]. "text/plain" is default *)
    ; filename: string option  (** [filename] form field attribute. *)
    ; parameters: string Map.t
          (** Additional [key = value] params of the form field. *) }

  val pp : Format.formatter -> t -> unit
    [@@ocaml.toplevel_printer]
  (** [pp fmt part] is the pretty printer for [t]. *)

  val equal : t -> t -> bool
  (** [equal part1 part2] returns [true] if [part1] and [part2] are equal. *)
end

type t = Part.t list Map.t
(** Represents a parsed HTTP [multipart/form-data] request as a [key/value] map.
    Submitted form field name is the key value.

    A key may be associated in zero or more values.*)

exception Multipart of string
(** Represents error while parsing http multipart formdata. *)

(** {2 Parse} *)

val parse : content_type_header:string -> body:string -> t
(** [parse ~content_type_header ~body] returns a parsed HTTP multiparts such
    that it can be queried using ocaml [Stdlib.Map] functions.

    [content_type_header] is the HTTP request [Content-Type] header. Note the
    value contains both the header name and value. It is used to parse a
    [boundary] value.

    [body] is the raw HTTP POST request body content.

    {4 Examples}

    {[
      let content_type_header =
        "Content-Type: multipart/form-data; \
         boundary=---------------------------735323031399963166993862150" in
      let body =
        [ {||}; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="text1"|}; {||}
        ; {|text default|}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="text2"|}; {||}; {|aωb|}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
        ; {|Content-Type: text/plain|}; {||}; {|Content of a.txt.|}; {||}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file2"; filename="a.html"|}
        ; {|Content-Type: text/html|}; {||}
        ; {|<!DOCTYPE html><title>Content of a.html.</title>|}; {||}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
        ; {|Content-Type: application/octet-stream|}; {||}; {|aωb|}
        ; {|-----------------------------735323031399963166993862150--|} ]
        |> String.concat "\r\n" in
      let mp = Multipart.parse ~content_type_header ~body in
      let file1_1 = Multipart.Map.find "file1" mp in
      let file1_2 =
        [ { Multipart.Part.body= Bytes.of_string "\r\nContent of a.txt.\r\n\r\n"
          ; name= "file1"
          ; content_type= "text/plain"
          ; filename= Some "a.txt"
          ; parameters= Multipart.Map.empty } ] in
      Multipart.equal_parts file1_1 file1_2
    ]}
    @raise Multipart *)

(** {2 Pretty Printers} *)

val pp_parts : Format.formatter -> Part.t list -> unit
  [@@ocaml.toplevel_printer]
(** [pp_parts fmt parts] pretty prints a list of [Part.t] *)

val pp : Format.formatter -> t -> unit
  [@@ocaml.toplevel_printer]
(** [pp fmt part] pretty prints a [part]. *)

(** {2 Equals} *)

val equal_parts : Part.t list -> Part.t list -> bool
(** [equal_parts parts1 parts2] returns [true] if [parts1] and [parts2] are
    equal, [false] otherwise. *)

val equal : t -> t -> bool
(** [equal t1 t2] returns [true] if [Part.] [t1] and [t2] are equal, [false]
    otherwise. *)
