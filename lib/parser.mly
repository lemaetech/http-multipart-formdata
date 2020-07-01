%token MULTIPART_FORMDATA
%token SEMI
%token EOF
%token <string> BOUNDARY_VALUE
%start <string> parse_content_type
%%

parse_content_type :
| MULTIPART_FORMDATA SEMI b = BOUNDARY_VALUE EOF {b}
| MULTIPART_FORMDATA SEMI b = BOUNDARY_VALUE SEMI {b}
