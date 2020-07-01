open Http_multipart_formdata

let print_boundary_value s =
  Multipart_formdata.parse_content_type s
  |> Multipart_formdata.sexp_of_result
  |> Sexplib0.Sexp.to_string_hum
  |> print_endline


let%expect_test "name" =
  [
    "multipart/form-data; boundary=gc0p4Jq0M2Yt08j34c0p";
    {|multipart/form-data; boundary="gc0p4Jq0M2Yt08j34c0p:=???|"|};
    "multipart/form-data; boundary=gc0p4Jq0M2Yt08j34c0p ";
    "multipart/form-data; boundary=---gc0p4Jq0M2Yt08j34c0p ";
    "multipart/form-data; boundary=---gc0p4Jq0M2Yt08j34c0p helll";
    "";
  ]
  |> List.iter print_boundary_value;
  [%expect
    {|
    (Ok gc0p4Jq0M2Yt08j34c0p)
    (Ok gc0p4Jq0M2Yt08j34c0p:=???|)
    (Ok gc0p4Jq0M2Yt08j34c0p)
    (Ok ---gc0p4Jq0M2Yt08j34c0p)
    (Ok "---gc0p4Jq0M2Yt08j34c0p helll")
    (Error Invalid_content_type) |}]
