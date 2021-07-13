(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

module Map = struct
  include Map.Make (String)

  let pp pp_value fmt t =
    let pp_kv = Fmt.pair ~sep:Fmt.comma Fmt.string pp_value in
    let pp_kv fmt pv = Fmt.pf fmt "@[(%a)@]" pp_kv pv in
    Fmt.seq ~sep:Fmt.semi pp_kv fmt (to_seq t)
end

type input =
  [ `Stream of char Lwt_stream.t
  | `Fd of Lwt_unix.file_descr
  | `Channel of Lwt_io.input_channel ]

type boundary = string
(** Represents the multipart boundary value. *)

type part_header = {
  name : string;
  content_type : string;
  filename : string option;
  parameters : string Map.t;
}

let name t = t.name

let content_type t = t.content_type

let filename t = t.filename

let param_value name t = Map.find_opt name t.parameters

let compare_part_header (a : part_header) (b : part_header) = compare a b

let equal_part_header (a : part_header) (b : part_header) = compare a b = 0

let pp_part_header fmt part =
  let fields =
    [
      Fmt.field "name" (fun p -> p.name) Fmt.string;
      Fmt.field "content_type" (fun p -> p.content_type) Fmt.string;
      Fmt.field "filename" (fun p -> p.filename) Fmt.(option string);
      Fmt.field "parameters" (fun p -> p.parameters) (Map.pp Fmt.string);
    ]
  in
  Fmt.record ~sep:Fmt.semi fields fmt part

module Make_common (P : Reparse.PARSER) = struct
  open P

  let is_space c = c == '\x20'

  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  let is_alpha_digit = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let implode l = List.to_seq l |> String.of_seq

  let is_tspecial = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
    | ']' | '?' | '=' ->
        true
    | _ -> false

  let is_ascii_char = function '\x00' .. '\x7F' -> true | _ -> false

  let is_token_char c =
    is_ascii_char c
    && (not (is_space c))
    && (not (is_control c))
    && not (is_tspecial c)

  let token =
    let+ chars = take ~at_least:1 (char_if is_token_char) <?> "[token]" in
    implode chars

  let is_qtext = function
    | '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' -> true
    | _ -> false

  (* https://tools.ietf.org/html/rfc5322#section-3.2.1 quoted-pair = ('\' (VCHAR
     / WSP)) / obs-qp *)
  let quoted_pair = char '\\' *> (whitespace <|> vchar) <$> String.make 1

  let quoted_string =
    let qtext = char_if is_qtext <$> String.make 1 in
    let qcontent =
      take (qtext <|> quoted_pair) <$> fun l -> String.concat "" l
    in
    dquote *> qcontent <* dquote

  let param_value = token <|> quoted_string
end

let parse_boundary ~content_type =
  let open Reparse.String in
  let open Make_common (Reparse.String) in
  let boundary =
    let is_bcharnospace = function
      | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?'
        ->
          true
      | c when is_alpha_digit c -> true
      | _ -> false
    in
    let bchars =
      char_if (function
        | '\x20' -> true
        | c when is_bcharnospace c -> true
        | _ -> false)
    in
    let boundary =
      let* bchars = take ~up_to:70 bchars in
      let len = List.length bchars in
      if len > 0 then
        let last_char = List.nth bchars (len - 1) in
        if is_bcharnospace last_char then return (implode bchars)
        else fail "Invalid boundary value: invalid last char"
      else fail "Invalid boundary value: 0 length"
    in
    optional dquote *> boundary <* optional dquote <|> token
  in
  let param =
    let* attribute = skip whitespace *> char ';' *> skip whitespace *> token in
    let+ value =
      char '=' *> if attribute = "boundary" then boundary else param_value
    in
    (attribute, value)
  in
  skip whitespace
  *> (string_cs "multipart/form-data" <?> "Not multipart formdata header")
  *> skip whitespace *> take param
  >>= (fun params ->
        match List.assoc_opt "boundary" params with
        | Some b -> return b
        | None -> fail "'boundary' parameter not found")
  |> parse (create_input_from_string content_type)
  |> Result.map (fun (x, _) -> x)

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

  val parse_parts :
    ?part_stream_chunk_size:int ->
    boundary:boundary ->
    on_part:(part_header -> part_body_stream:char Lwt_stream.t -> unit Lwt.t) ->
    input ->
    (unit * int, string) result Lwt.t

  val parse_part : reader -> read_result Lwt.t
end

module Make (P : Reparse.PARSER with type 'a promise = 'a Lwt.t) :
  MULTIPART_PARSER with type input = P.input with type 'a t = 'a P.t = struct
  open P

  open Make_common (P)

  type input = P.input

  type 'a t = 'a P.t

  let param =
    let name = skip whitespace *> char ';' *> skip whitespace *> token in
    let value = char '=' *> param_value in
    (name, value) <$$> fun name value -> (name, value)

  let p_restricted_name =
    let p_restricted_name_chars =
      char_if (function
        | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
        | c when is_alpha_digit c -> true
        | _ -> false)
    in
    let* first_ch = char_if is_alpha_digit in
    let buf = Buffer.create 10 in
    Buffer.add_char buf first_ch;
    let+ restricted_name = take ~up_to:126 p_restricted_name_chars in
    Buffer.add_string buf (implode restricted_name);
    Buffer.contents buf

  type part_body_header =
    | Content_type of {
        ty : string;
        subtype : string;
        parameters : string Map.t;
      }
    | Content_disposition of string Map.t

  let content_disposition =
    let+ params =
      string_cs "Content-Disposition:"
      *> skip whitespace *> string_cs "form-data" *> take param
    in
    let params = List.to_seq params |> Map.of_seq in
    Content_disposition params

  let content_type parse_header_name =
    let* ty =
      (if parse_header_name then string_cs "Content-Type:" *> unit else unit)
      *> skip whitespace *> p_restricted_name
    in
    let* subtype = char '/' *> p_restricted_name in
    let+ params = take param in
    let parameters = params |> List.to_seq |> Map.of_seq in
    Content_type { ty; subtype; parameters }

  let part_body_header =
    take ~at_least:1 ~sep_by:crlf
      (any [ content_disposition; content_type true ])
    <* crlf
    >>= fun headers ->
    let name, content_type, filename, parameters =
      List.fold_left
        (fun (name, ct, filename, params) header ->
          match header with
          | Content_type ct ->
              let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
              ( name,
                content_type,
                filename,
                Map.union (fun _key a _b -> Some a) params ct.parameters )
          | Content_disposition params2 ->
              let name = Map.find_opt "name" params2 in
              let filename = Map.find_opt "filename" params2 in
              ( name,
                ct,
                filename,
                Map.union (fun _key a _b -> Some a) params params2 ))
        (None, None, None, Map.empty)
        headers
    in
    match name with
    | None -> fail "Invalid part. parameter 'name' not found"
    | Some name ->
        let content_type = Option.value content_type ~default:"text/plain" in
        let parameters = Map.remove "name" parameters in
        let parameters =
          match filename with
          | Some _ -> Map.remove "filename" parameters
          | None -> parameters
        in
        return { name; content_type; filename; parameters }

  let parse_parts ?(part_stream_chunk_size = 1024 * 1024) ~boundary ~on_part
      http_body =
    let boundary_type =
      let body_end = string_cs "--" *> optional crlf $> `End in
      let part_start = string_cs "\r\n" $> `Part_start in
      body_end <|> part_start <?> "Invalid 'multipart/formdata' boundary value"
    in
    let dash_boundary = string_cs @@ Format.sprintf "--%s" boundary in
    let crlf_dash_boundary = crlf *> dash_boundary in
    (* ---- 
       l_parts - list of part promises
     * ----*)
    let rec loop_parts l_parts =
      let* boundary_type' = boundary_type <* trim_input_buffer in
      match boundary_type' with
      | `End -> of_promise (Lwt.join l_parts)
      | `Part_start ->
          let* header = part_body_header <* trim_input_buffer in
          let stream, push = Lwt_stream.create_bounded part_stream_chunk_size in
          let part_p = on_part header ~part_body_stream:stream in
          take_while_cb unsafe_any_char ~while_:(is_not crlf_dash_boundary)
            ~on_take_cb:(fun x -> of_promise @@ push#push x)
          *> trim_input_buffer
          >>= fun () ->
          (push#close;
           unit)
          *> crlf_dash_boundary
          *> loop_parts (part_p :: l_parts)
    in
    (*** Ignore preamble - any text before first boundary value. ***)
    take_while_cb ~while_:(is_not dash_boundary)
      ~on_take_cb:(fun (_c : char) -> unit)
      unsafe_any_char
    *> dash_boundary *> trim_input_buffer *> loop_parts []
    |> parse http_body

  type reader = {
    input : input;
    boundary : boundary;
    read_body_len : int;
    mutable pos : int;
    mutable parsing_body : bool;
    mutable preamble_parsed : bool;
  }

  and read_result =
    [ `End
    | `Header of part_header
    | `Body of bigstring * int
    | `Error of string ]

  and bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let preamble boundary =
    let dash_boundary = string_cs @@ Format.sprintf "--%s" boundary in
    (*** Ignore preamble - any text before first boundary value. ***)
    take_while_cb ~while_:(is_not dash_boundary)
      ~on_take_cb:(fun (_c : char) -> unit)
      unsafe_any_char
    *> dash_boundary *> trim_input_buffer

  let _index_of ~affix buf =
    let not_matched = -1 in
    let max_alen = String.length affix - 1 in
    let max_blen = Cstruct.length buf - 1 in
    let rec loop idx_a idx_b matched_from_idx =
      if idx_a > max_alen || idx_b > max_blen then matched_from_idx
      else
        let ch_a = String.unsafe_get affix idx_a in
        let ch_b = Cstruct.get_char buf idx_b in
        if Char.equal ch_a ch_b then
          let matched_from_idx =
            if matched_from_idx = not_matched then idx_b else matched_from_idx
          in
          loop (idx_a + 1) (idx_b + 1) matched_from_idx
        else loop 0 (idx_b + 1) not_matched
    in
    loop 0 0 not_matched

  let part_header =
    take ~at_least:1 (crlf *> any [ content_disposition; content_type true ])
    >>= fun headers ->
    let name, content_type, filename, parameters =
      List.fold_left
        (fun (name, ct, filename, params) header ->
          match header with
          | Content_type ct ->
              let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
              ( name,
                content_type,
                filename,
                Map.union (fun _key a _b -> Some a) params ct.parameters )
          | Content_disposition params2 ->
              let name = Map.find_opt "name" params2 in
              let filename = Map.find_opt "filename" params2 in
              ( name,
                ct,
                filename,
                Map.union (fun _key a _b -> Some a) params params2 ))
        (None, None, None, Map.empty)
        headers
    in
    match name with
    | None -> fail "Invalid part header. Parameter 'name' not found"
    | Some name ->
        let content_type = Option.value content_type ~default:"text/plain" in
        let parameters = Map.remove "name" parameters in
        let parameters =
          match filename with
          | Some _ -> Map.remove "filename" parameters
          | None -> parameters
        in
        let header = { name; content_type; filename; parameters } in
        return (`Header header)

  let rec part reader =
    (if not reader.preamble_parsed then preamble reader.boundary else unit)
    >>= fun () ->
    if reader.parsing_body then part_body reader
    else
      let end_ = string_cs "--" *> optional crlf $> `End in
      let part_body = crlf *> crlf *> part_body reader in
      end_ <|> part_header <|> part_body

  and part_body reader =
    let* _buf = unsafe_take_cstruct_ne reader.read_body_len in
    (* read until crlf_dash_boundary *)
    return `End

  (* let boundary_type = *)
  (*   let body_end = string_cs "--" *> optional crlf $> `End in *)
  (*   let part_start = string_cs "\r\n" $> `Part_start in *)
  (*   body_end <|> part_start <?> "Invalid 'multipart/formdata' boundary value" *)
  (* in *)
  (* let dash_boundary = string_cs @@ Format.sprintf "--%s" reader.boundary in *)
  (* let crlf_dash_boundary = crlf *> dash_boundary in *)
  (* let* boundary_type' = boundary_type <* trim_input_buffer in *)
  (* match boundary_type' with *)
  (* | `End -> return `End *)
  (* | `Part_start -> *)
  (*     let* _header = part_body_header <* trim_input_buffer in *)
  (*     let* buf = unsafe_take_cstruct_ne reader.read_body_len in *)
  (*     let* () = trim_input_buffer <* crlf_dash_boundary in *)
  (*     return (`Body (Cstruct.to_bigarray buf, Cstruct.length buf)) *)

  let reader ?(read_body_len = 0) boundary input =
    {
      input;
      pos = 0;
      read_body_len;
      boundary;
      parsing_body = false;
      preamble_parsed = false;
    }

  let parse_part (reader : reader) : read_result Lwt.t =
    Lwt.(
      parse reader.input (part reader) >|= function
      | Ok (a, pos) ->
          reader.pos <- pos;
          a
      | Error e -> `Error e)
end

let rec parse_parts ?part_stream_chunk_size ~boundary ~on_part
    (http_body : input) =
  Lwt_result.(
    (match http_body with
    | `Stream stream ->
        parse_parts_stream ?part_stream_chunk_size ~boundary ~on_part stream
    | `Fd fd -> parse_parts_fd ?part_stream_chunk_size ~boundary ~on_part fd
    | `Channel channel ->
        parse_parts_channel ?part_stream_chunk_size ~boundary ~on_part channel)
    >|= fun (x, _) -> x)

and parse_parts_stream ?part_stream_chunk_size ~boundary ~on_part http_body =
  let module P = Make (Reparse_lwt.Stream) in
  let http_body = Reparse_lwt.Stream.create_input http_body in
  P.parse_parts ?part_stream_chunk_size ~boundary ~on_part http_body

and parse_parts_fd ?part_stream_chunk_size ~boundary ~on_part http_body =
  let module P = Make (Reparse_lwt_unix.Fd) in
  let http_body = Reparse_lwt_unix.Fd.create_input http_body in
  P.parse_parts ?part_stream_chunk_size ~boundary ~on_part http_body

and parse_parts_channel ?part_stream_chunk_size ~boundary ~on_part http_body =
  let module P = Make (Reparse_lwt_unix.Channel) in
  let http_body = Reparse_lwt_unix.Channel.create_input http_body in
  P.parse_parts ?part_stream_chunk_size ~boundary ~on_part http_body
