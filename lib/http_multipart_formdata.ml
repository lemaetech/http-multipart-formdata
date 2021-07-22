open Angstrom

module Map = struct
  include Map.Make (String)

  let pp pp_value fmt t =
    let pp_kv = Fmt.pair ~sep:Fmt.comma Fmt.string pp_value in
    let pp_kv fmt pv = Fmt.pf fmt "@[(%a)@]" pp_kv pv in
    Fmt.seq ~sep:Fmt.semi pp_kv fmt (to_seq t)
end

type boundary = Boundary of string [@@unboxed]

and reader =
  {mutable parser_state: read_result Angstrom.Buffered.state; state: state}

and state =
  { dash_boundary: string
  ; crlf_dash_boundary: string
  ; read_body_len: int
  ; mutable parsing_body: bool
  ; mutable preamble_parsed: bool }

and read_result =
  [ `End
  | `Header of part_header
  | `Body of Cstruct.t
  | `Body_end
  | `Awaiting_input of
    [`String of string | `Bigstring of bigstring | `Eof] -> read_result
  | `Error of string ]

and part_header =
  { name: string
  ; content_type: string
  ; filename: string option
  ; parameters: string Map.t }

and bigstring = Bigstringaf.t

type part_body_header =
  | Content_type of {ty: string; subtype: string; parameters: string Map.t}
  | Content_disposition of string Map.t

let name (p : part_header) = p.name
let content_type p = p.content_type
let filename p = p.filename
let find name p = Map.find_opt name p.parameters

let pp_part_header fmt part =
  let fields =
    [ Fmt.field "name" (fun p -> p.name) Fmt.string
    ; Fmt.field "parameters" (fun p -> p.parameters) (Map.pp Fmt.string)
    ; Fmt.field "content_type" (fun p -> p.content_type) Fmt.string
    ; Fmt.field "filename" (fun p -> p.filename) Fmt.(option string) ]
  in
  Fmt.record ~sep:Fmt.semi fields fmt part

let pp_boundary fmt (Boundary boundary) = Fmt.string fmt boundary
let is_space c = c == '\x20'
let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

let is_alpha_digit = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

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

let token = take_while1 is_token_char <?> "[token]"

let is_qtext = function
  | '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' -> true
  | _ -> false

let whitespace = satisfy (function ' ' | '\t' -> true | _ -> false)
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let dquote = char '"'

(* https://tools.ietf.org/html/rfc5322#section-3.2.1 quoted-pair = ('\' (VCHAR /
   WSP)) / obs-qp *)
let quoted_pair = String.make 1 <$> char '\\' *> (whitespace <|> vchar)

let quoted_string =
  let qtext = String.make 1 <$> satisfy is_qtext in
  let qcontent = many (qtext <|> quoted_pair) >>| fun l -> String.concat "" l in
  dquote *> qcontent <* dquote

let param_value = token <|> quoted_string

let param =
  let name =
    skip_many whitespace *> char ';' *> skip_many whitespace *> token
  in
  let value = char '=' *> param_value in
  lift2 (fun name value -> (name, value)) name value

let restricted_name =
  let restricted_name_chars = function
    | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
    | c when is_alpha_digit c -> true
    | _ -> false
  in
  let* first_ch = satisfy is_alpha_digit in
  let count = ref 0 in
  let+ restricted_name =
    take_while (fun c ->
        if !count < 126 && restricted_name_chars c then (incr count ; true)
        else false )
  in
  Format.sprintf "%c%s" first_ch restricted_name

let optional x = option None (x >>| Option.some)

let boundary ~content_type =
  let boundary_param_value =
    let is_bcharnospace = function
      | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?'
        ->
          true
      | c when is_alpha_digit c -> true
      | _ -> false
    in
    let bchars = function
      | '\x20' -> true
      | c when is_bcharnospace c -> true
      | _ -> false
    in
    let boundary_val =
      let* bchars =
        let count = ref 0 in
        take_while (fun c ->
            if !count < 70 && bchars c then (incr count ; true) else false )
      in
      let len = String.length bchars in
      if len > 0 then
        let last_char = bchars.[len - 1] in
        if is_bcharnospace last_char then return bchars
        else fail "Invalid boundary value: invalid last char"
      else fail "Invalid boundary value: 0 length"
    in
    optional dquote *> boundary_val <* optional dquote <|> token
  in
  let param =
    let* attribute =
      skip_many whitespace *> char ';' *> skip_many whitespace *> token
    in
    let+ value =
      char '='
      *> if attribute = "boundary" then boundary_param_value else param_value
    in
    (attribute, value)
  in
  let p =
    skip_many whitespace
    *> (string_ci "multipart/form-data" <?> "Not multipart formdata header")
    *> skip_many whitespace *> many param
    >>= fun params ->
    match List.assoc_opt "boundary" params with
    | Some boundary -> return (Boundary boundary)
    | None -> fail "'boundary' parameter not found"
  in
  parse_string ~consume:Consume.All p content_type

let content_disposition =
  let+ params =
    string_ci "Content-Disposition:"
    *> skip_many whitespace *> string_ci "form-data" *> many param
  in
  let params = List.to_seq params |> Map.of_seq in
  Content_disposition params

let unit = return ()

let pp_read_result : Format.formatter -> read_result -> unit =
 fun fmt ->
  let pp fmt = function
    | `End -> Fmt.string fmt "End"
    | `Header header -> Fmt.fmt "Header: %a" fmt pp_part_header header
    | `Body buf ->
        Fmt.fmt "Body: %d, %s" fmt (Cstruct.length buf)
          (Cstruct.to_string buf |> String.escaped)
    | `Body_end -> Fmt.string fmt "Body_end"
    | `Awaiting_input _ -> ()
    | `Error e -> Fmt.fmt "Error %s" fmt e
  in
  Fmt.(vbox (pp ++ cut)) fmt

(* ignore all text before first boundary value. *)
let preamble dash_boundary =
  let len = String.length dash_boundary in
  let p =
    peek_string len
    >>= fun dash_boudary' ->
    if String.equal dash_boundary dash_boudary' then fail "" else any_char
  in
  p *> advance len *> commit

let crlf = string_ci "\r\n" <?> "[crlf]"

let part_header =
  let content_type =
    let* ty =
      string_ci "Content-Type:" *> skip_many whitespace *> restricted_name
    in
    let* subtype = char '/' *> restricted_name in
    let+ params = many param in
    let parameters = params |> List.to_seq |> Map.of_seq in
    Content_type {ty; subtype; parameters}
  in
  many1 (crlf *> choice [content_disposition; content_type])
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

let rec part state =
  ( if not state.preamble_parsed then
    preamble state.dash_boundary >>| fun () -> state.preamble_parsed <- true
  else unit )
  >>= fun () ->
  if state.parsing_body then part_body state
  else
    let end_ = string "--" *> optional crlf *> return `End in
    let part_body =
      let* () = crlf *> crlf *> unit in
      state.parsing_body <- true ;
      part_body state
    in
    end_ <|> part_header <|> part_body <* commit

and part_body state : read_result t =
  let buf = Cstruct.create state.read_body_len in
  let rec read_part_body i =
    if i < state.read_body_len then (
      let* is_boundary =
        let len = String.length state.crlf_dash_boundary in
        let+ crlf_dash_boundary' = peek_string len in
        String.equal crlf_dash_boundary' state.crlf_dash_boundary
      in
      if is_boundary then
        if i = 0 then (
          let* () = string state.crlf_dash_boundary *> unit in
          state.parsing_body <- false ;
          return `Body_end )
        else
          let buf' = Cstruct.sub buf 0 i in
          return @@ `Body buf'
      else
        let* ch = any_char in
        Cstruct.set_char buf i ch ;
        (read_part_body [@tailcall]) (i + 1) )
    else return @@ `Body buf
  in
  read_part_body 0

let reader ?(read_body_len = 1024) (Boundary boundary) =
  let crlf_dash_boundary = Format.sprintf "\r\n--%s" boundary in
  let read_body_len = max read_body_len (String.length crlf_dash_boundary) in
  let crlf_dash_boundary = crlf_dash_boundary in
  let dash_boundary = Format.sprintf "--%s" boundary in
  let state =
    { read_body_len
    ; dash_boundary
    ; crlf_dash_boundary
    ; parsing_body= false
    ; preamble_parsed= false }
  in
  let parser_state = Buffered.parse (part state) in
  {parser_state; state}

let rec read_part (reader : reader) =
  match reader.parser_state with
  | Buffered.Partial k ->
      let continue input =
        let parser_state = k input in
        reader.parser_state <- parser_state ;
        read_part reader
      in
      `Awaiting_input continue
  | Buffered.Done (_, a) -> a
  | Buffered.Fail (_, _, e) -> `Error e
