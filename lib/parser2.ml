open Std
open R.O

type error =
  [ `Msg of string
  | `Expected_char of int * char
  | `Expected_string of int * string
  | `Eof of int ]

type state = {
  src : src;
  len : int;
  mutable offset : int;
  mutable cc : current_char;
}

and src = [ `String of string | `Bigstring of Bigstringaf.t ]

and current_char = [ `Char of char | `Eof ]

type (+'a, +'error) t = state -> (state * 'a, state * 'error) result

let msgf state fmt = Format.kasprintf (fun s -> R.error (state, `Msg s)) fmt

let pp_current_char fmt = function
  | `Char c -> Format.fprintf fmt "'%c'" c
  | `Eof -> Format.fprintf fmt "EOF"

let advance n state =
  let current_char state =
    `Char
      ( match state.src with
      | `String src -> src.[state.offset]
      | `Bigstring src -> Bigstringaf.unsafe_get src state.offset )
  in
  if state.offset + n < state.len then (
    state.offset <- state.offset + n;
    state.cc <- current_char state )
  else (
    state.offset <- state.len;
    state.cc <- `Eof )

let of_string s (t : ('a, 'e) t) =
  let src = `String s in
  let len = String.length s in
  let state = { src; len; offset = -1; cc = `Eof } in
  advance 1 state;
  match t state with Ok (_, a) -> Ok a | Error (_, e) -> Error e

let substring len state =
  if state.offset + len < state.len then
    ( match state.src with
    | `String src -> String.sub src ~pos:state.offset ~len
    | `Bigstring src -> Bigstringaf.substring src ~off:state.offset ~len )
    |> R.ok
  else R.error @@ (state, `Eof state.offset)

let char c state =
  if state.cc = `Char c then (
    advance 1 state;
    R.ok (state, c) )
  else
    msgf state "%d: char '%c' expected instead of %a" state.offset c
      pp_current_char state.cc

let string s state =
  let len = String.length s in
  let* s2 = substring len state in
  if s = s2 then (
    advance len state;
    R.ok (state, s) )
  else R.error @@ (state, `Expected_string (state.offset, s))

let ok v state = R.ok (state, v)

let error e state = R.error (state, e)

let rec skip_while f state =
  match state.cc with
  | `Char c ->
      if f c then (
        advance 1 state;
        skip_while f state )
      else ok () state
  | `Eof -> ok () state

let ( *> ) (p : (_, 'error) t) (q : ('a, 'error) t) state =
  p state >>= fun (state, _) -> q state

let ( >>= ) t f state = t state >>= fun (_, a) -> f a state
