open Http_multipart_formdata

let print_boundary_value s =
  Multipart_formdata.parse_content_type s
  |> (function
       | Ok b -> b
       | Error e -> Multipart_formdata.string_of_error e)
  |> print_endline


let%expect_test "name" =
  [ "multipart/form-data; boundary=gc0p4Jq0M2Yt08j34c0p"
  ; {|multipart/form-data; boundary="gc0p4Jq0M2Yt08j34c0p:=???|"|}
  ; "multipart/form-data; boundary=gc0p4Jq0M2Yt08j34c0p "
  ; "multipart/form-data; boundary=---gc0p4Jq0M2Yt08j34c0p "
  ; "multipart/form-data; boundary=---gc0p4Jq0M2Yt08j34c0p helll"
  ; "multipart/form-data; boundary=---gc0p4Jq0M2Yt08j34c0p; hello=2"
  ; ""
  ]
  |> List.iter print_boundary_value;
  [%expect
    {|
    gc0p4Jq0M2Yt08j34c0p
    gc0p4Jq0M2Yt08j34c0p:=???|
    gc0p4Jq0M2Yt08j34c0p
    ---gc0p4Jq0M2Yt08j34c0p
    ---gc0p4Jq0M2Yt08j34c0p helll
    ---gc0p4Jq0M2Yt08j34c0p
    Invalid_content_type |}]
