open Std
open Sexplib.Std

module Bigstringaf = struct
  include Bigstringaf

  let sexp_of_t t = Sexplib.Sexp.of_bigstring t
end

type src = String of string | Bigstring of Bigstringaf.t [@@deriving sexp_of]

type t = {
  src : src;
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

let get_src_char i t =
  ( match t.src with
  | String s -> s.[i]
  | Bigstring bstr -> Bigstringaf.unsafe_get bstr i )
  |> Char_token.of_char

let next t =
  if t.rd_offset < t.src_len then (
    t.offset <- t.rd_offset;
    t.ch <- get_src_char t.rd_offset t;
    t.rd_offset <- t.rd_offset + 1 )
  else (
    t.offset <- t.src_len;
    t.ch <- Char_token.eof )

let peek t =
  if t.rd_offset < t.src_len then get_src_char t.rd_offset t else Char_token.eof

let peek2 t =
  if t.rd_offset + 1 < t.src_len then get_src_char (t.rd_offset + 1) t
  else Char_token.eof

let get_src_len = function
  | String s -> String.length s
  | Bigstring bstr -> Bigstringaf.length bstr

let create src =
  let t =
    {
      src;
      src_len = get_src_len src;
      ch = Char_token.eof;
      offset = 0;
      start_offset = 0;
      rd_offset = 0;
    }
  in
  next t;
  t

let of_string s =
  let src = String s in
  create src

let of_bigstring s =
  let src = Bigstring s in
  create src

let lexeme t =
  let off = t.start_offset in
  let len = t.offset - t.start_offset in
  match t.src with
  | String src -> String.sub src ~pos:off ~len
  | Bigstring src -> Bigstringaf.substring src ~off ~len

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
