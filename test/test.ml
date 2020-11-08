open Http_multipart_formdata

let parts = Alcotest.testable Multipart.pp_parts Multipart.equal_parts

let single_value_suite =
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
  let mp = Multipart.parse ~content_type_header ~body in
  let file1_1 = Multipart.Map.find "file1" mp in
  let file1_2 =
    [ { Multipart.Part.body = Bytes.of_string "\r\nContent of a.txt.\r\n\r\n"
      ; name = "file1"
      ; content_type = "text/plain"
      ; filename = Some "a.txt"
      ; parameters = Multipart.Map.empty } ]
  in
  let file2_1 = Multipart.Map.find "file2" mp in
  let file2_2 =
    [ { Multipart.Part.body =
          Bytes.of_string
            "\r\n<!DOCTYPE html><title>Content of a.html.</title>\r\n\r\n"
      ; name = "file2"
      ; content_type = "text/html"
      ; filename = Some "a.html"
      ; parameters = Multipart.Map.empty } ]
  in
  let file3_1 = Multipart.Map.find "file3" mp in
  let file3_2 =
    [ { Multipart.Part.body = Bytes.of_string "\r\na\207\137b\r\n"
      ; name = "file3"
      ; content_type = "application/octet-stream"
      ; filename = Some "binary"
      ; parameters = Multipart.Map.empty } ]
  in
  let text1_1 = Multipart.Map.find "text1" mp in
  let text1_2 =
    [ { Multipart.Part.body = Bytes.of_string "\r\ntext default\r\n"
      ; name = "text1"
      ; content_type = "text/plain"
      ; filename = None
      ; parameters = Multipart.Map.empty } ]
  in
  let text2_1 = Multipart.Map.find "text2" mp in
  let text2_2 =
    [ { Multipart.Part.body = Bytes.of_string "\r\na\207\137b\r\n"
      ; name = "text2"
      ; content_type = "text/plain"
      ; filename = None
      ; parameters = Multipart.Map.empty } ]
  in
  [ ("file1", `Quick, fun () -> Alcotest.check parts "equal" file1_1 file1_2)
  ; ("file2", `Quick, fun () -> Alcotest.check parts "equal" file2_1 file2_2)
  ; ("file3", `Quick, fun () -> Alcotest.check parts "equal" file3_1 file3_2)
  ; ("text1", `Quick, fun () -> Alcotest.check parts "equal" text1_1 text1_2)
  ; ("text2", `Quick, fun () -> Alcotest.check parts "equal" text2_1 text2_2) ]

let multi_values_suite =
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
  let mp = Multipart.parse ~content_type_header ~body in
  let files_actual = Multipart.Map.find "file1" mp in
  let files_expected =
    [ { Multipart.Part.body = Bytes.of_string "\r\nContent of a.txt.\r\n\r\n"
      ; name = "file1"
      ; content_type = "text/plain"
      ; filename = Some "a.txt"
      ; parameters = Multipart.Map.empty }
    ; { Multipart.Part.body =
          Bytes.of_string
            "\r\n<!DOCTYPE html><title>Content of a.html.</title>\r\n\r\n"
      ; name = "file1"
      ; content_type = "text/html"
      ; filename = Some "a.html"
      ; parameters = Multipart.Map.empty }
    ; { Multipart.Part.body = Bytes.of_string "\r\naωb\r\n"
      ; name = "file1"
      ; content_type = "application/octet-stream"
      ; filename = Some "binary"
      ; parameters = Multipart.Map.empty } ]
  in
  let text1_a = Multipart.Map.find "text1" mp in
  let text1_e =
    [ { Multipart.Part.body = Bytes.of_string "\r\ntext default\r\n"
      ; name = "text1"
      ; content_type = "text/plain"
      ; filename = None
      ; parameters = Multipart.Map.empty }
    ; { Multipart.Part.body = Bytes.of_string "\r\naωb\r\n"
      ; name = "text1"
      ; content_type = "text/plain"
      ; filename = None
      ; parameters = Multipart.Map.empty } ]
  in
  [ ( "file1"
    , `Quick
    , fun () -> Alcotest.check parts "equal" files_actual files_expected )
  ; ("text1", `Quick, fun () -> Alcotest.check parts "equal" text1_a text1_e) ]

let () =
  Printexc.record_backtrace true ;
  Alcotest.run
    "Multipart"
    [("Single Values", single_value_suite); ("Multi Values", multi_values_suite)]
