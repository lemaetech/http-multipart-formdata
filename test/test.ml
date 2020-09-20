open Http_multipart_formdata

let%expect_test _ =
  let content_type_header =
    "Content-Type: multipart/form-data; \
     boundary=---------------------------735323031399963166993862150"
  in
  let body =
    [ {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text1"|}
    ; {||}
    ; {|text default|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text2"|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
    ; {|Content-Type: text/plain|}
    ; {||}
    ; {|Content of a.txt.|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file2"; filename="a.html"|}
    ; {|Content-Type: text/html|}
    ; {||}
    ; {|<!DOCTYPE html><title>Content of a.html.</title>|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
    ; {|Content-Type: application/octet-stream|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150--|} ]
    |> String.concat "\r\n"
  in
  Multipart_formdata.(
    parse ~content_type_header ~body |> pp Format.std_formatter) ;
  [%expect
    {|
    ((file1
       ((File
          ((filename (a.txt)) (content_type text/plain) (parameters ())
            (body  "Content of a.txt.\r\
                  \n\r\
                  \n")))))
      (file2
        ((File
           ((filename (a.html)) (content_type text/html) (parameters ())
             (body  "<!DOCTYPE html><title>Content of a.html.</title>\r\
                   \n\r\
                   \n")))))
      (file3
        ((File
           ((filename (binary)) (content_type application/octet-stream)
             (parameters ()) (body "a\207\137b\r\n")))))
      (text1 ((String "text default\r\n"))) (text2 ((String "a\207\137b\r\n"))))|}]

let%expect_test "multiple body parts with same form field." =
  let content_type_header =
    " multipart/form-data; \
     boundary=---------------------------735323031399963166993862150"
  in
  let body =
    [ {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text1"|}
    ; {||}
    ; {|text default|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text1"|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
    ; {|Content-Type: text/plain|}
    ; {||}
    ; {|Content of a.txt.|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="a.html"|}
    ; {|Content-Type: text/html|}
    ; {||}
    ; {|<!DOCTYPE html><title>Content of a.html.</title>|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="binary"|}
    ; {|Content-Type: application/octet-stream|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150--|} ]
    |> String.concat "\r\n"
  in
  Multipart_formdata.(
    parse ~content_type_header ~body |> pp Format.std_formatter) ;
  [%expect
    {|
    ((file1
       ((File
          ((filename (a.txt)) (content_type text/plain) (parameters ())
            (body  "Content of a.txt.\r\
                  \n\r\
                  \n")))
         (File
           ((filename (a.html)) (content_type text/html) (parameters ())
             (body  "<!DOCTYPE html><title>Content of a.html.</title>\r\
                   \n\r\
                   \n")))
         (File
           ((filename (binary)) (content_type application/octet-stream)
             (parameters ()) (body "a\207\137b\r\n")))))
      (text1 ((String "text default\r\n") (String "a\207\137b\r\n")))) |}]

module SM = Multipart_formdata.String_map

let%test "find/body_parts" =
  let content_type_header =
    " multipart/form-data; \
     boundary=---------------------------735323031399963166993862150"
  in
  let body =
    [ {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text1"|}
    ; {||}
    ; {|text default|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="text1"|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
    ; {|Content-Type: text/plain|}
    ; {||}
    ; {|Content of a.txt.|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="a.html"|}
    ; {|Content-Type: text/html|}
    ; {||}
    ; {|<!DOCTYPE html><title>Content of a.html.</title>|}
    ; {||}
    ; {|-----------------------------735323031399963166993862150|}
    ; {|Content-Disposition: form-data; name="file1"; filename="binary"|}
    ; {|Content-Type: application/octet-stream|}
    ; {||}
    ; {|aωb|}
    ; {|-----------------------------735323031399963166993862150--|} ]
    |> String.concat "\r\n"
  in
  match Multipart_formdata.parse ~content_type_header ~body with
  | parts       ->
      List.length (SM.find "text1" parts) = 2
      && List.length (SM.find "file1" parts) = 3
      && SM.cardinal parts = 2
  | exception _ -> false
