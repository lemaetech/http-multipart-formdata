open Std
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

let is_vchar = function '\x21' .. '\x7E' -> true | _ -> false

let is_whitespace = function '\x20' | '\x09' -> true | _ -> false

let is_ctext = function
  | '\x21' .. '\x27' | '\x2A' .. '\x5B' | '\x5D' .. '\x7E' -> true
  | _ -> false

let is_qtext = function
  | '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' -> true
  | _ -> false

let whitespace = skip_while is_whitespace

let token =
  let token_char c =
    is_ascii_chars c
    && (not (is_space c))
    && (not (is_control c))
    && not (is_tspecials c)
  in
  satisfy token_char >>= fun ch ->
  take_while token_char >>= fun chars -> String.make 1 ch ^ chars |> ok

(* https://tools.ietf.org/html/rfc5322#section-3.2.1
   quoted-pair     =   ('\' (VCHAR / WSP)) / obs-qp
*)
let quoted_pair =
  char '\\' *> satisfy (fun c -> is_whitespace c || is_vchar c) >>| fun c ->
  String.make 1 '\\' ^ String.make 1 c

(* https://tools.ietf.org/html/rfc5322#section-3.2.2 *)
let fws =
  count_skip_while is_whitespace >>= fun ws_count1 ->
  count_skip_while_string 3 (fun s ->
      String.equal s "\x0D\x0A\x20" || String.equal s "\x0D\x0A\x09")
  >>= fun lws_count ->
  count_skip_while is_whitespace >>| fun ws_count2 ->
  if ws_count1 + lws_count + ws_count2 > 0 then " " else ""

let comment =
  let ctext = satisfy is_ctext >>| String.make 1 in
  let rec loop_comment () =
    char '('
    *> many
         ( fws >>= fun sp ->
           ctext
           <|> quoted_pair
           <|> (loop_comment () >>| fun txt -> "(" ^ txt ^ ")")
           >>| ( ^ ) sp )
    >>| String.concat ~sep:""
    >>= fun comment_text ->
    fws >>= fun sp -> ok @@ comment_text ^ sp <* char ')'
  in
  loop_comment ()

let cfws =
  many
    ( fws >>= fun sp ->
      comment >>| fun comment_text -> sp ^ comment_text )
  >>= (fun l ->
        fws >>| fun sp -> if String.length sp > 0 then l @ [ sp ] else l)
  <|> (fws >>| fun sp -> if String.length sp > 0 then [ sp ] else [])

let quoted_string =
  let qcontent = satisfy is_qtext >>| String.make 1 <|> quoted_pair in
  cfws *> char '"' *> many (fws >>= fun sp -> qcontent >>| ( ^ ) sp)
  >>| String.concat ~sep:""
  >>= fun q_string -> fws >>| (fun sp -> q_string ^ sp) <* char '"'

let param =
  whitespace *> char ';' *> whitespace *> token >>= fun attribute ->
  char '=' *> (token <|> quoted_string) >>| fun value -> (attribute, value)

let restricted_name =
  let is_restricted_name_chars = function
    | '!' | '#' | '$' | '&' | '-' | '^' | '_' | '.' | '+' -> true
    | c when is_alpha_digit c -> true
    | _ -> false
  in
  satisfy is_alpha_digit >>= fun first_ch ->
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
