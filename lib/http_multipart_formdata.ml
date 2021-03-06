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

type part_header =
  { name: string
  ; content_type: string
  ; filename: string option
  ; parameters: string Map.t }

(** Represents the multipart boundary value. *)
and boundary = Boundary of string [@@unboxed]

let name t = t.name
let content_type t = t.content_type
let filename t = t.filename
let param_value name t = Map.find_opt name t.parameters

let pp_part_header fmt part =
  let fields =
    [ Fmt.field "name" (fun p -> p.name) Fmt.string
    ; Fmt.field "parameters" (fun p -> p.parameters) (Map.pp Fmt.string)
    ; Fmt.field "content_type" (fun p -> p.content_type) Fmt.string
    ; Fmt.field "filename" (fun p -> p.filename) Fmt.(option string) ]
  in
  Fmt.record ~sep:Fmt.semi fields fmt part

let pp_boundary fmt (Boundary boundary) = Fmt.string fmt boundary

module type MULTIPART_PARSER = sig
  type 'a t

  and input

  and 'a promise

  and reader

  and read_result =
    [ `End
    | `Header of part_header
    | `Body of Cstruct.t
    | `Body_end
    | `Error of string ]

  val reader : ?read_body_len:int -> boundary -> input -> reader
  val read_part : reader -> read_result promise
  val pp_read_result : Format.formatter -> read_result -> unit
end

module Make (P : Reparse.PARSER) = struct
  open P

  type input = P.input

  and 'a t = 'a P.t

  and 'a promise = 'a P.promise

  and reader =
    { input: input
    ; dash_boundary: string t
    ; crlf_dash_boundary: string t
    ; read_body_len: int
    ; mutable pos: int
    ; mutable parsing_body: bool
    ; mutable preamble_parsed: bool }

  and read_result =
    [ `End
    | `Header of part_header
    | `Body of Cstruct.t
    | `Body_end
    | `Error of string ]

  let is_space c = c == '\x20'
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  let is_alpha_digit = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let implode l = List.to_seq l |> String.of_seq

  let is_tspecial = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
     |']' | '?' | '=' ->
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

  let param =
    let name = skip whitespace *> char ';' *> skip whitespace *> token in
    let value = char '=' *> param_value in
    (name, value) <$$> fun name value -> (name, value)

  let p_restricted_name =
    let p_restricted_name_chars =
      char_if (function
        | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
        | c when is_alpha_digit c -> true
        | _ -> false )
    in
    let* first_ch = char_if is_alpha_digit in
    let buf = Buffer.create 10 in
    Buffer.add_char buf first_ch ;
    let+ restricted_name = take ~up_to:126 p_restricted_name_chars in
    Buffer.add_string buf (implode restricted_name) ;
    Buffer.contents buf

  let boundary =
    let boundary_param_value =
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
          | _ -> false )
      in
      let boundary_val =
        let* bchars = take ~up_to:70 bchars in
        let len = List.length bchars in
        if len > 0 then
          let last_char = List.nth bchars (len - 1) in
          if is_bcharnospace last_char then return (implode bchars)
          else fail "Invalid boundary value: invalid last char"
        else fail "Invalid boundary value: 0 length"
      in
      optional dquote *> boundary_val <* optional dquote <|> token
    in
    let param =
      let* attribute =
        skip whitespace *> char ';' *> skip whitespace *> token
      in
      let+ value =
        char '='
        *> if attribute = "boundary" then boundary_param_value else param_value
      in
      (attribute, value)
    in
    skip whitespace
    *> (string_cs "multipart/form-data" <?> "Not multipart formdata header")
    *> skip whitespace *> take param
    >>= fun params ->
    match List.assoc_opt "boundary" params with
    | Some boundary -> return (Boundary boundary)
    | None -> fail "'boundary' parameter not found"

  type part_body_header =
    | Content_type of {ty: string; subtype: string; parameters: string Map.t}
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
    Content_type {ty; subtype; parameters}

  let pp_read_result : Format.formatter -> read_result -> unit =
   fun fmt ->
    let pp fmt = function
      | `End -> Fmt.string fmt "End"
      | `Header header -> Fmt.fmt "Header: %a" fmt pp_part_header header
      | `Body buf ->
          Fmt.fmt "Body: %d, %s" fmt (Cstruct.length buf)
            (Cstruct.to_string buf |> String.escaped)
      | `Body_end -> Fmt.string fmt "Body_end"
      | `Error e -> Fmt.fmt "Error %s" fmt e
    in
    Fmt.(vbox (pp ++ cut)) fmt

  (* ignore all text before first boundary value. *)
  let preamble reader =
    take_while_cb
      ~while_:(is_not reader.dash_boundary)
      ~on_take_cb:(fun (_c : char) -> unit)
      unsafe_any_char
    *> reader.dash_boundary *> trim_input_buffer

  let part_header =
    take ~at_least:1 (crlf *> any [content_disposition; content_type true])
    >>= fun headers ->
    let name, content_type, filename, parameters =
      List.fold_left
        (fun (name, ct, filename, params) header ->
          match header with
          | Content_type ct ->
              let content_type = Some (ct.ty ^ "/" ^ ct.subtype) in
              ( name
              , content_type
              , filename
              , Map.union (fun _key a _b -> Some a) params ct.parameters )
          | Content_disposition params2 ->
              let name = Map.find_opt "name" params2 in
              let filename = Map.find_opt "filename" params2 in
              ( name
              , ct
              , filename
              , Map.union (fun _key a _b -> Some a) params params2 ) )
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
        let header = {name; content_type; filename; parameters} in
        return (`Header header)

  let rec part reader =
    ( if not reader.preamble_parsed then
      preamble reader >>| fun () -> reader.preamble_parsed <- true
    else unit )
    >>= fun () ->
    if reader.parsing_body then part_body reader
    else
      let end_ = string_cs "--" *> optional crlf $> `End in
      let part_body =
        let* () = crlf *> crlf *> unit in
        reader.parsing_body <- true ;
        part_body reader
      in
      end_ <|> part_header <|> part_body <* trim_input_buffer

  and part_body reader : read_result t =
    let buf = Cstruct.create reader.read_body_len in
    let rec read_part_body i =
      if i < reader.read_body_len then (
        let* is_boundary = is reader.crlf_dash_boundary in
        if is_boundary then
          if i = 0 then (
            let* () = reader.crlf_dash_boundary *> unit in
            reader.parsing_body <- false ;
            return `Body_end )
          else
            let buf' = Cstruct.sub buf 0 i in
            return @@ `Body buf'
        else
          let* ch = unsafe_any_char in
          Cstruct.set_char buf i ch ;
          (read_part_body [@tailcall]) (i + 1) )
      else return @@ `Body buf
    in
    read_part_body 0

  let reader ?(read_body_len = 1024) (Boundary boundary) input =
    let crlf_dash_boundary = Format.sprintf "\r\n--%s" boundary in
    let read_body_len = max read_body_len (String.length crlf_dash_boundary) in
    let crlf_dash_boundary = string_cs crlf_dash_boundary in
    let dash_boundary = string_cs @@ Format.sprintf "--%s" boundary in
    { input
    ; pos= 0
    ; read_body_len
    ; dash_boundary
    ; crlf_dash_boundary
    ; parsing_body= false
    ; preamble_parsed= false }

  let read_part (reader : reader) : read_result promise =
    Promise.(
      parse ~pos:reader.pos reader.input (part reader)
      >>| function
      | Ok (a, pos) ->
          reader.pos <- pos ;
          a
      | Error e -> `Error e)
end

let parse_boundary ~content_type =
  let module P = Make (Reparse.String) in
  Reparse.String.(parse (create_input_from_string content_type)) P.boundary
  |> Result.map (fun (x, _) -> x)
