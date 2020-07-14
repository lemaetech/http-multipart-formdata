open Std
open R.O

type error = [ `Msg of string ]

type state = { src : src; len : int; offset : int; cc : current_char }

and src = [ `String of string | `Bigstring of Bigstringaf.t ]

and current_char = [ `Char of char | `Eof ]

type ('a, 'error) t = state -> (state * 'a, state * 'error) result

let msgf state fmt = Format.kasprintf (fun s -> R.error (state, `Msg s)) fmt

let pp_current_char fmt = function
  | `Char c -> Format.fprintf fmt "%c" c
  | `Eof -> Format.fprintf fmt "EOF"

let ( <|> ) p q state = match p state with Ok _ as o -> o | Error _ -> q state

let ( *> ) (p : (_, 'error) t) (q : ('a, 'error) t) state =
  R.bind (p state) (fun (state, _) -> q state)

let ( <* ) (p : ('a, 'error) t) (q : (_, 'error) t) state =
  R.bind (q state) (fun (state, _) -> p state)

let ( *>| ) p a state = R.map (fun (state, _) -> (state, a)) (p state)

let ( >>= ) t f state = t state >>= fun (state, a) -> f a state

let ( >>| ) (t : ('a, 'error) t) (f : 'a -> 'b) state =
  t state >>| fun (state, a) -> (state, f a)

let advance n state =
  let current_char offset =
    `Char
      ( match state.src with
      | `String src -> src.[offset]
      | `Bigstring src -> Bigstringaf.unsafe_get src offset )
  in
  if state.offset + n < state.len then
    let offset = state.offset + n in
    let state = { state with offset; cc = current_char offset } in
    R.ok (state, ())
  else
    let state = { state with offset = state.len; cc = `Eof } in
    R.ok (state, ())

let of_string s t =
  let src = `String s in
  let len = String.length s in
  let state = { src; len; offset = -1; cc = `Eof } in
  R.bind (advance 1 state) (fun (state, ()) -> t state) |> function
  | Ok (_, a) -> Ok a
  | Error (_, e) -> Error e

let substring len state =
  if state.offset + len < state.len then
    ( match state.src with
    | `String src -> String.sub src ~pos:state.offset ~len
    | `Bigstring src -> Bigstringaf.substring src ~off:state.offset ~len )
    |> Option.some
  else None

let ok v state = R.ok (state, v)

let fail e state = R.error (state, e)

let char c state =
  if state.cc = `Char c then
    R.map (fun (state, ()) -> (state, c)) (advance 1 state)
  else
    msgf state "%d: char '%c' expected instead of '%a'" state.offset c
      pp_current_char state.cc

let satisfy f state =
  match state.cc with
  | `Char c when f c ->
      R.bind (advance 1 state) (fun (state, ()) -> R.ok (state, c))
  | `Char _ | `Eof ->
      msgf state "%d: char_if returned 'false' for char '%a'" state.offset
        pp_current_char state.cc

let peek_char state =
  let v = match state.cc with `Char c -> Some c | `Eof -> None in
  R.ok (state, v)

let peek_char_fail state =
  match state.cc with
  | `Char c -> R.ok (state, c)
  | `Eof -> msgf state "%d: peek_char_fail returned EOF" state.offset

let any_char state =
  match state.cc with
  | `Char c -> R.bind (advance 1 state) (fun (state, ()) -> R.ok (state, c))
  | `Eof -> msgf state "%d: any_char returned EOF" state.offset

let peek_string n state = R.ok (state, substring n state)

let string s state =
  let len = String.length s in
  match substring len state with
  | Some s2 ->
      if s = s2 then R.map (fun (state, ()) -> (state, s)) (advance len state)
      else msgf state "%d: string \"%s\" not found" state.offset s
  | None -> msgf state "%d: got EOF while parsing string \"%s\"" state.offset s

let rec skip_while f state =
  match satisfy f state with
  | Ok (state, _) -> skip_while f state
  | Error (state, _) -> ok () state

let count_skip_while f state =
  let rec loop count state =
    match satisfy f state with
    | Ok (state, _) -> loop (count + 1) state
    | Error (state, _) -> ok count state
  in
  loop 0 state

let count_skip_while_string n f =
  let rec loop count =
    peek_string n >>= function
    | Some s -> if f s then advance n *> loop (count + 1) else ok count
    | None -> ok count
  in
  loop 0

let take_while f state =
  let rec loop buf state =
    match satisfy f state with
    | Ok (state, c) ->
        Buffer.add_char buf c;
        loop buf state
    | Error (state, _) -> R.ok (state, Buffer.contents buf)
  in
  loop (Buffer.create 10) state

let many t state =
  let rec loop l state =
    match t state with
    | Ok (state, a) -> loop (a :: l) state
    | Error _ -> (state, l)
  in
  let state, v = loop [] state in
  ok (List.rev v) state

let count_skip_many t state =
  let rec loop count state =
    match t state with
    | Ok (state, _) -> loop (count + 1) state
    | Error _ -> (state, count)
  in
  let state, v = loop 0 state in
  ok v state

let take_while_n n f state =
  let rec loop count buf state =
    if count < n then
      match satisfy f state with
      | Ok (state, c) ->
          Buffer.add_char buf c;
          loop (count + 1) buf state
      | Error (state, _) -> R.ok (state, Buffer.contents buf)
    else R.ok (state, Buffer.contents buf)
  in
  loop 0 (Buffer.create n) state
