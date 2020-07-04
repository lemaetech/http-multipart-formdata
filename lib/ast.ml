type multipart_formdata =
  { parts : multipart_body_part list
  ; close_boundary : string
  }

and multipart_body_part =
  { dash_boundary : string
  ; headers : multipart_body_header list
  ; body : string option
  }

and multipart_body_header =
  { header : multipart_body_header_type
  ; params : (string * string) list
  }

and multipart_body_header_type =
  [ `Content_type of string
  | `Content_disposition of string
  ]
