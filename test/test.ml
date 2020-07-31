open Sexplib0

type ('a, 'error) t = ('a, 'error) result = Ok of 'a | Error of 'error
[@@deriving sexp_of]

let pp fmt t =
  sexp_of_t Http_multipart_formdata.sexp_of_t
    Http_multipart_formdata.sexp_of_error t
  |> Sexp.pp_hum_indent 2 fmt

let%expect_test _ =
  let header =
    "Content-Type: multipart/form-data; \
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
  Http_multipart_formdata.parse ~header ~body:(`String body)
  |> pp Format.std_formatter;

  [%expect
    {|
    (Ok
      ((file1
         (((name file1) (filename (a.txt)) (content_type text/plain)
            (parameters ()) (body  "\r\
                                  \n\r\
                                  \nContent of a.txt.\r\
                                  \n\r\
                                  \n"))))
        (file2
          (((name file2) (filename (a.html)) (content_type text/html)
             (parameters ())
             (body
                "\r\
               \n\r\
               \n<!DOCTYPE html><title>Content of a.html.</title>\r\
               \n\r\
               \n"))))
        (file3
          (((name file3) (filename (binary))
             (content_type application/octet-stream) (parameters ())
             (body  "\r\
                   \n\r\
                   \na\207\137b\r\
                   \n"))))
        (text1
          (((name text1) (filename ()) (content_type text/plain) (parameters ())
             (body  "\r\
                   \n\r\
                   \ntext default\r\
                   \n"))))
        (text2
          (((name text2) (filename ()) (content_type text/plain) (parameters ())
             (body  "\r\
                   \n\r\
                   \na\207\137b\r\
                   \n"))))))|}]

let%expect_test "multiple body parts with same form field." =
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
      {|Content-Disposition: form-data; name="text1"|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="a.txt"|};
      {|Content-Type: text/plain|};
      {||};
      {|Content of a.txt.|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="a.html"|};
      {|Content-Type: text/html|};
      {||};
      {|<!DOCTYPE html><title>Content of a.html.</title>|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="binary"|};
      {|Content-Type: application/octet-stream|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150--|};
    ]
    |> String.concat "\r\n"
  in
  Http_multipart_formdata.parse ~header ~body:(`String body)
  |> pp Format.std_formatter;

  [%expect
    {|
    (Ok
      ((file1
         (((name file1) (filename (a.txt)) (content_type text/plain)
            (parameters ()) (body  "\r\
                                  \n\r\
                                  \nContent of a.txt.\r\
                                  \n\r\
                                  \n"))
           ((name file1) (filename (a.html)) (content_type text/html)
             (parameters ())
             (body
                "\r\
               \n\r\
               \n<!DOCTYPE html><title>Content of a.html.</title>\r\
               \n\r\
               \n"))
           ((name file1) (filename (binary))
             (content_type application/octet-stream) (parameters ())
             (body  "\r\
                   \n\r\
                   \na\207\137b\r\
                   \n"))))
        (text1
          (((name text1) (filename ()) (content_type text/plain) (parameters ())
             (body  "\r\
                   \n\r\
                   \ntext default\r\
                   \n"))
            ((name text1) (filename ()) (content_type text/plain) (parameters ())
              (body  "\r\
                    \n\r\
                    \na\207\137b\r\
                    \n")))))) |}]

let%test "find" =
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
      {|Content-Disposition: form-data; name="text1"|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="a.txt"|};
      {|Content-Type: text/plain|};
      {||};
      {|Content of a.txt.|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="a.html"|};
      {|Content-Type: text/html|};
      {||};
      {|<!DOCTYPE html><title>Content of a.html.</title>|};
      {||};
      {|-----------------------------735323031399963166993862150|};
      {|Content-Disposition: form-data; name="file1"; filename="binary"|};
      {|Content-Type: application/octet-stream|};
      {||};
      {|aωb|};
      {|-----------------------------735323031399963166993862150--|};
    ]
    |> String.concat "\r\n"
  in
  Http_multipart_formdata.parse ~header ~body:(`String body) |> function
  | Ok parts ->
      List.length (Http_multipart_formdata.find "text1" parts) = 2
      && List.length (Http_multipart_formdata.find "file1" parts) = 3
      && List.length (Http_multipart_formdata.body_parts parts) = 5
  | Error _ -> false
