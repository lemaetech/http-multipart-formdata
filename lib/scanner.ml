type mode =
  | Multipart_formdata
  | Multipart_body_part
  | Multipart_body_header_param

type t =
  { src : bytes
  ; src_len : int
  ; mutable ch : int (* current character *)
  ; mutable offset : int (* character offset *)
  ; mutable rd_offset : int (* reading offset (position after current character) *)
  }

module Char_code = struct
  let space = 0x20
  let null = 0x00
  let invalid = -1
end

let next scanner =
  if scanner.rd_offset < scanner.src_len
  then (
    scanner.offset <- scanner.rd_offset;
    let ch = int_of_char @@ Bytes.get scanner.src scanner.rd_offset in
    scanner.rd_offset <- scanner.rd_offset + 1;
    scanner.ch <- ch)
  else (
    scanner.offset <- scanner.src_len;
    scanner.ch <- Char_code.invalid)


let peek scanner =
  if scanner.rd_offset < scanner.src_len
  then Bytes.unsafe_get scanner.src scanner.rd_offset |> int_of_char
  else Char_code.invalid


let create b =
  let scanner =
    { src = b; src_len = Bytes.length b; ch = Char_code.null; offset = 0; rd_offset = 0 }
  in
  next scanner;
  scanner
