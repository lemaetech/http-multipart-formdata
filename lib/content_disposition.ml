open Parser2

let is_alpha_digit = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let crlf = string "\x0D\x0A"

let is_space c = c == '\x20'

let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

let is_tspecials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '[' | ']'
  | '?' | '=' ->
      true
  | _ -> false

let is_ascii_chars = function '\x00' .. '\x7F' -> true | _ -> false

let whitespace = skip_while (fun c -> c == '\x20' || c == '\x09')

let token =
  let token_char c =
    is_ascii_chars c
    && (not (is_space c))
    && (not (is_control c))
    && not (is_tspecials c)
  in

  char_if token_char >>= fun ch ->
  take_while token_char >>= fun chars -> String.make 1 ch ^ chars |> ok

let param =
  char ';' *> whitespace *> token >>= fun attribute ->
  char '=' *> token >>= fun value -> ok (attribute, value)

let restricted_name =
  let is_restricted_name_chars = function
    | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
    | c when is_alpha_digit c -> true
    | _ -> false
  in

  char_if is_alpha_digit >>= fun first_ch ->
  let buf = Buffer.create 10 in
  Buffer.add_char buf first_ch;
  take_while_n 126 is_restricted_name_chars >>= fun restricted_name ->
  Buffer.add_string buf restricted_name;
  ok @@ Buffer.contents buf

let parse s =
  crlf
  *> string "Content-Disposition"
  *> char ':'
  *> whitespace
  *> string "form-data"
  *> many param
  |> of_string s
