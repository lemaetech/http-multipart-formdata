(* RFC - https://tools.ietf.org/html/rfc2046#section-5.1.1 *)
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
let boundary = bchars* bcharsnospace
let sp = ['\032']

rule lex_boundary = parse
| (sp*) "boundary=\"" (boundary as b) "\"" {  b }
| (sp*) "boundary=" (boundary as b) (eof | sp+ | ';' ) { b }