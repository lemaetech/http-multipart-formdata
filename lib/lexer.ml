open Std

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
  mutable mode : 'mode; (* current lexer mode. *)
}

type state = { ch : int; offset : int; rd_offset : int }

let lex_start (t : 'mode t) = t.start_offset <- t.offset

let state (t : 'mode t) =
  { ch = t.ch; offset = t.offset; rd_offset = t.rd_offset }

let set_state (state : state) (t : 'mode t) =
  t.offset <- state.offset;
  t.rd_offset <- state.rd_offset;
  t.ch <- state.ch

let next (t : 'mode t) =
  if t.rd_offset < t.src_len then (
    t.offset <- t.rd_offset;
    t.ch <- int_of_char t.src.[t.rd_offset];
    t.rd_offset <- t.rd_offset + 1 )
  else (
    t.offset <- t.src_len;
    t.ch <- Char_code.invalid )

let peek (t : 'mode t) =
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

let expect ch (t : 'mode t) =
  if t.ch == ch then (
    next t;
    R.ok () )
  else sprintf "expected '%03d' but got '%03d'" ch t.ch |> R.error

let accept ch (t : 'mode t) =
  if t.ch == ch then (
    next t;
    lexeme t |> R.ok )
  else sprintf "expected '%03d' but got '%03d'" ch t.ch |> R.error
