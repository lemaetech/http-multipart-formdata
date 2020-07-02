%token MULTIPART_FORMDATA
%token SEMI
%token EOF
%token <string> BOUNDARY_VALUE
%type  <string> parse_content_type
%start parse_content_type
%%

parse_content_type :
| MULTIPART_FORMDATA SEMI BOUNDARY_VALUE EOF {$3}
| MULTIPART_FORMDATA SEMI BOUNDARY_VALUE SEMI {$3}
