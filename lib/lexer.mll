(* RFC - https://tools.ietf.org/html/rfc2046#section-5.1.1 *)
{
  open Parser
}
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let specials = [',' '(' ')' '+' '_' ',' '-' '.' '|' ':' '=' '?']

let bcharsnospace =  digit | alpha | specials
let bchars = bcharsnospace | ' '
let boundary = bchars* bcharsnospace
let ws = [' ' '\t' ]

rule lex_content_type = parse
| [' ' '\t'] {lex_content_type lexbuf}
| ';' { SEMI }
| "multipart/form-data" {MULTIPART_FORMDATA}
| "boundary" (ws)* '=' (ws)* {lex_boundary lexbuf}
| eof {EOF}
| _ {lex_content_type lexbuf}

and lex_boundary = parse
| "\"" (boundary as b) "\""  {BOUNDARY_VALUE b }
| (boundary as b) {BOUNDARY_VALUE b}