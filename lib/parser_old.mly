%token MULTIPART_FORMDATA
%token SEMI
%token EOF
%token CRLF
%token <string> BOUNDARY_VALUE
%token <string> DASH_BOUNDARY
%token <string> CLOSE_BOUNDARY
%token <Ast.multipart_body_header_type> HEADER
%token <string * string> HEADER_PARAM
%token <string> BODY

%start <string> parse_content_type
%start <Ast.multipart_formdata> parse_multipart_formdata
%%

parse_content_type :
| MULTIPART_FORMDATA; SEMI; b = BOUNDARY_VALUE; EOF {b}
| MULTIPART_FORMDATA; SEMI; b = BOUNDARY_VALUE; SEMI EOF {b}

parse_multipart_formdata :
| parts = parse_body_part+; close_boundary = CLOSE_BOUNDARY EOF
  { {Ast.parts; close_boundary} }

parse_body_part :
| dash_boundary = DASH_BOUNDARY; headers = parse_body_part_header*; body = BODY?
  { {Ast.dash_boundary; headers; body} }

parse_body_part_header:
| header = HEADER; params = HEADER_PARAM* CRLF
  { {Ast.header; params} }
