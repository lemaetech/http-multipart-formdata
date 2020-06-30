open Http_multipart_formdata

let print_boundary_value s =
  Multipart_formdata.parse_content_type s
  |> Multipart_formdata.sexp_of_result
  |> Sexplib0.Sexp.to_string_hum
  |> print_endline


let%expect_test "name" =
  [
    "gc0p4Jq0M2Yt08j34c0p";
    {|"gc0p4Jq0M2Yt08j34c0p:=???|"|};
    "gc0p4Jq0M2Yt08j34c0p ";
    "---gc0p4Jq0M2Yt08j34c0p ";
    "";
  ]
  |> List.map (( ^ ) "multipart/form-data; boundary=")
  |> List.iter print_boundary_value;
  [%expect
    {|
    (Ok gc0p4Jq0M2Yt08j34c0p)
    (Ok gc0p4Jq0M2Yt08j34c0p:=???|)
    (Ok gc0p4Jq0M2Yt08j34c0p)
    (Ok ---gc0p4Jq0M2Yt08j34c0p)
    (Error Invalid_boundary_value) |}]
