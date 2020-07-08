open Std
open Sexplib.Std

type t = {
  src : string;
  src_len : int;
  (* current character *)
  mutable ch : Char_token.t;
  (* character offset *)
  mutable offset : int;
  (* start offset *)
  mutable start_offset : int;
  (* reading offset (position after current character) *)
  mutable rd_offset : int;
}
[@@deriving sexp_of]

let pp fmt t = sexp_of_t t |> Sexplib.Sexp.pp_hum_indent 2 fmt

let current t = t.ch

let lex_start t = t.start_offset <- t.offset

let next t =
  if t.rd_offset < t.src_len then (
    t.offset <- t.rd_offset;
    t.ch <- Char_token.of_char t.src.[t.rd_offset];
    t.rd_offset <- t.rd_offset + 1 )
  else (
    t.offset <- t.src_len;
    t.ch <- Char_token.eof )

let peek t =
  if t.rd_offset < t.src_len then Char_token.of_char t.src.[t.rd_offset]
  else Char_token.eof

let peek2 t =
  if t.rd_offset + 1 < t.src_len then Char_token.of_char t.src.[t.rd_offset + 1]
  else Char_token.eof

let create src =
  let t =
    {
      src;
      src_len = String.length src;
      ch = Char_token.eof;
      offset = 0;
      start_offset = 0;
      rd_offset = 0;
    }
  in
  next t;
  t

let lexeme t = String.sub t.src t.start_offset (t.offset - t.start_offset)

let expect ch t =
  if t.ch == ch then (
    next t;
    R.ok () )
  else
    asprintf "expected '%a' but got '%a'" Char_token.pp ch Char_token.pp t.ch
    |> R.error

let accept ch t =
  if t.ch == ch then (
    lex_start t;
    next t;
    lexeme t |> R.ok )
  else
    asprintf "expected '%a' but got '%a'" Char_token.pp ch Char_token.pp t.ch
    |> R.error
