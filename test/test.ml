open Sexplib0
open Http_multipart_formdata

type ('a, 'error) t = ('a, 'error) result = Ok of 'a | Error of 'error
[@@deriving sexp_of]

let pp fmt t =
  let pp1 = Sexp_conv.sexp_of_list Multipart_formdata.sexp_of_t in
  sexp_of_t pp1 Multipart_formdata.sexp_of_error t |> Sexp.pp_hum_indent 2 fmt

let%expect_test _ =
  let header =
    " multipart/form-data; \
     boundary=---------------------------735323031399963166993862150"
  in
  let body =
    [
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="text1"|};
      {||};
      {|text default|};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="text2"|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="a.txt"|};
      {|Content-Type: text/plain|};
      {||};
      {|Content of a.txt.|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file2"; filename="a.html"|};
      {|Content-Type: text/html|};
      {||};
      {|<!DOCTYPE html><title>Content of a.html.</title>|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file3"; filename="binary"|};
      {|Content-Type: application/octet-stream|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150--|};
    ]
    |> String.concat "\r\n"
  in
  Multipart_formdata.parse ~header ~body:(`String body)
  |> pp Format.std_formatter;

  [%expect {|
    (Ok
      (((form_field file3) (filename (binary)) (content_type text/plain)
         (parameters ((filename binary) (name file3)))
         (body
            "\r\
           \nContent-Type: application/octet-stream\r\
           \n\r\
           \na\207\137b\r\
           \n"))
        ((form_field file1) (filename (a.txt)) (content_type text/plain)
          (parameters ((filename a.txt) (name file1)))
          (body
             "\r\
            \nContent-Type: text/plain\r\
            \n\r\
            \nContent of a.txt.\r\
            \n\r\
            \n"))
        ((form_field text1) (filename ()) (content_type text/plain)
          (parameters ((name text1))) (body  "\r\
                                            \n\r\
                                            \ntext default\r\
                                            \n"))))|}]
