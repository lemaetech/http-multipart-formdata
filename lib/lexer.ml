type mode =
  | Multipart_formdata
  | Multipart_body_part
  | Multipart_body_header_param

type t =
  { src : string
  ; src_len : int
  ; mutable ch : int (* current character *)
  ; mutable offset : int (* character offset *)
  ; mutable rd_offset : int (* reading offset (position after current character) *)
  }

module Char_code = struct
  let space = 0x20
  let htab = 0x09
  let null = 0x00
  let invalid = -1
  let is_vchar c = c >= 0x21 && c <= 0x7E
end

let next t =
  if t.rd_offset < t.src_len
  then (
    t.offset <- t.rd_offset;
    t.ch <- int_of_char t.src.[t.rd_offset];
    t.rd_offset <- t.rd_offset + 1)
  else (
    t.offset <- t.src_len;
    t.ch <- Char_code.invalid)


let peek t =
  if t.rd_offset < t.src_len
  then int_of_char @@ String.unsafe_get t.src t.rd_offset
  else Char_code.invalid


let create b =
  let t =
    { src = b; src_len = String.length b; ch = Char_code.null; offset = 0; rd_offset = 0 }
  in
  next t;
  t


let rec lex_whitespace t =
  if t.ch == Char_code.space || t.ch == Char_code.htab
  then (
    next t;
    lex_whitespace t)
  else ()


let lex_field_value t =
  let start_offs = t.offset in
  let rec lex () =
    if Char_code.is_vchar t.ch
    then (
      next t;
      lex ())
    else if t.ch = Char_code.space || t.ch = Char_code.htab
    then (
      lex_whitespace t;
      if Char_code.is_vchar @@ peek t
      then (
        next t;
        lex ()))
  in
  lex ();
  let v = String.sub t.src start_offs (t.offset - start_offs) |> String.trim in
  Token.Header_field_value v


let lex_formdata _t = ()

(* tokenizes HTTP 'Content-Type' header value, eg. multipart/form-data; boundary=7353230 *)
let lex_multipart_header_value _t = ()

(*--------------------------------------------*)
(*----------------- Unit Tests ---------------*)
(*--------------------------------------------*)

let%expect_test _ =
  [ "gzip, deflate"; "feed"; "text/html" ]
  |> List.map create
  |> List.iter (fun lexer -> lex_field_value lexer |> Token.pp Format.std_formatter);
  [%expect
    {|
    (Header_field_value "gzip, deflate")(Header_field_value feed)(Header_field_value
                                                                  text/html) |}]
