open Http_multipart_formdata

let print_boundary_value s =
  let lb = Lexing.from_string s in
  Lexer.lex_boundary lb
  |> Lexer.sexp_of_result
  |> Sexplib0.Sexp.to_string_hum
  |> print_endline


let%expect_test "name" =
  [
    "gc0p4Jq0M2Yt08j34c0p";
    {|"gc0p4Jq0M2Yt08j34c0p:=???|"|};
    "gc0p4Jq0M2Yt08j34c0p ";
  ]
  |> List.map (( ^ ) "boundary=")
  |> List.iter print_boundary_value;
  [%expect
    {|
    (Ok c0p4Jq0M2Yt08j34c0p)
    (Ok gc0p4Jq0M2Yt08j34c0p:=???|)
    (Ok c0p4Jq0M2Yt08j34c0p) |}]
