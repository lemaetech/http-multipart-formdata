open Std
open R.O

type error = [ `Msg of string ]

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
  | `Char c -> Format.fprintf fmt "%c" c
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
  else R.error `Eof

let ok v state = R.ok (state, v)

let fail e state = R.error (state, e)

let char c state =
  if state.cc = `Char c then (
    advance 1 state;
    R.ok (state, c) )
  else
    msgf state "%d: char '%c' expected instead of %a" state.offset c
      pp_current_char state.cc

let char_if f state =
  match state.cc with
  | `Char c when f c ->
      advance 1 state;
      ok c state
  | `Char _ | `Eof ->
      msgf state "%d: char_if returned 'false' for char '%a'" state.offset
        pp_current_char state.cc

let string s state =
  let len = String.length s in
  match substring len state with
  | Ok s2 ->
      if s = s2 then (
        advance len state;
        R.ok (state, s) )
      else msgf state "%d: string \"%s\" not found" state.offset s
  | Error `Eof ->
      msgf state "%d: got EOF while parsing string \"%s\"" state.offset s

let rec skip_while f state =
  match state.cc with
  | `Char c ->
      if f c then (
        advance 1 state;
        skip_while f state )
      else ok () state
  | `Eof -> ok () state

let take_while f state =
  let rec loop buf =
    match state.cc with
    | `Char c when f c ->
        Buffer.add_char buf c;
        advance 1 state;
        loop buf
    | `Char _ | `Eof -> Buffer.contents buf
  in

  loop (Buffer.create 10) |> fun s -> ok s state

let many t state =
  let rec loop state l =
    match t state with Ok (state, a) -> loop state (a :: l) | Error _ -> l
  in

  let v = loop state [] in
  ok v state

let take_while_n n f state =
  let rec loop count buf =
    if count < n then
      match state.cc with
      | `Char c when f c ->
          Buffer.add_char buf c;
          advance 1 state;
          loop (count + 1) buf
      | `Char _ | `Eof -> Buffer.contents buf
    else Buffer.contents buf
  in

  loop 0 (Buffer.create n) |> fun s -> ok s state

let ( *> ) (p : (_, 'error) t) (q : ('a, 'error) t) state =
  p state >>= fun (state, _) -> q state

let ( >>= ) t f state = t state >>= fun (_, a) -> f a state
