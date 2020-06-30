(* RFC - https://tools.ietf.org/html/rfc2046#section-5.1.1 *)

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