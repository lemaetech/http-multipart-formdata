(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
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

type boundary = string

val parse_boundary : content_type:string -> (boundary, string) Lwt_result.t

(** [parse ~content_type_header ~body part_handler] parses [body] and streams
    [part_header] and [part_body_data] to [part_handler].

    [content_type_header] is the HTTP request [Content-Type] header value. It is
    used to parse a [boundary] value.

    [body] is the raw HTTP POST request body content stream.

    {4 Examples}

    {[
      module M = Http_multipart_formdata

      ;;
      let content_type_header =
        "multipart/form-data; \
         boundary=---------------------------735323031399963166993862150"
      in
      let body =
        [ {||}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="text1"|}
        ; {||}
        ; {|text default|}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="text2"|}
        ; {||}
        ; {|aωb|}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
        ; {|Content-Type: text/plain|}
        ; {||}
        ; {|Content of a.txt.|}
        ; {||}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file2"; filename="a.html"|}
        ; {|Content-Type: text/html|}
        ; {||}
        ; {|<!DOCTYPE html><title>Content of a.html.</title>|}
        ; {||}
        ; {|-----------------------------735323031399963166993862150|}
        ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
        ; {|Content-Type: application/octet-stream|}
        ; {||}
        ; {|aωb|}
        ; {|-----------------------------735323031399963166993862150--|}
        ]
        |> String.concat "\r\n"
      in
      let mp = M.parse ~content_type_header ~body in
      let file1_1 = M.Map.find "file1" mp in
      let file1_2 =
        [ { M.Part.body = Bytes.of_string "\r\nContent of a.txt.\r\n\r\n"
          ; name = "file1"
          ; content_type = "text/plain"
          ; filename = Some "a.txt"
          ; parameters = M.Map.empty
          }
        ]
      in
      M.equal_parts file1_1 file1_2
    ]} *)
val parse :
     ?part_body_buf_size:int
  -> boundary:boundary
  -> on_part:(Part_header.t -> char Lwt_stream.t -> unit Lwt.t)
  -> char Lwt_stream.t
  -> (unit, string) Lwt_result.t
