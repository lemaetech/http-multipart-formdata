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

type state

val create : 'mode -> string -> 'mode t

val lex_start : 'mode t -> unit

val state : 'mode t -> state

val set_state : state -> 'mode t -> unit

val next : 'mode t -> unit

val peek : 'mode t -> int

val peek2 : 'mode t -> int

val lexeme : 'mode t -> string

val expect : int -> 'mode t -> (unit, string) R.t

val accept : int -> 'mode t -> (string, string) R.t
