(* RFCs
  1. https://tools.ietf.org/html/rfc2046#section-5.1
  2. https://tools.ietf.org/html/rfc2045#section-5.1
  3. https://tools.ietf.org/html/rfc2046#section-5.1.1
*)
{
  open Sexplib.Std

  type error = [`Invalid_boundary_value] [@@deriving sexp_of]
  type ('a, 'e) result = ('a, 'e) Result.t =
    | Ok of 'a
    | Error of 'e
    [@@deriving sexp_of]

  let sexp_of_result = sexp_of_result  (sexp_of_string) (sexp_of_error)
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let specials = [',' '(' ')' '+' '_' ',' '-' '.' '|' ':' '=' '?']

let bcharsnospace =  digit | alpha | specials
let bchars = bcharsnospace | ' '

let quoted_text = bchars* bcharsnospace

let us_ascii = ['\000'-'\127']
let ctl = ['\000'-'\031' '\127']
let sp = ['\032']
let crlf = '\r' '\n'
let token = [^'\034'] (us_ascii # ctl # sp # specials)+

rule lex_boundary = parse
| "boundary=\"" (quoted_text as b) "\"" {  Result.ok b }
| "boundary=" (token as b) (eof | sp+ ) { Result.ok b }
| eof { Result.error `Invalid_boundary_value }