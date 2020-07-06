type 'mode t = {
  src : string;
  src_len : int;
  mutable ch : int;
  (* current character *)
  mutable offset : int;
  (* character offset *)
  mutable start_offset : int;
  (* start offset *)
  mutable rd_offset : int;
  (* reading offset (position after current character) *)
  mutable mode : 'mode;
}

let lex_start t = t.start_offset <- t.offset

let next t =
  if t.rd_offset < t.src_len then (
    t.offset <- t.rd_offset;
    t.ch <- int_of_char t.src.[t.rd_offset];
    t.rd_offset <- t.rd_offset + 1 )
  else (
    t.offset <- t.src_len;
    t.ch <- Char_code.invalid )

let peek t =
  if t.rd_offset < t.src_len then int_of_char t.src.[t.rd_offset]
  else Char_code.invalid

let create mode src =
  let t =
    {
      src;
      src_len = String.length src;
      ch = Char_code.invalid;
      offset = 0;
      start_offset = 0;
      rd_offset = 0;
      mode;
    }
  in
  next t;
  t

let lexeme t = String.sub t.src t.start_offset (t.offset - t.start_offset)
