open Http_multipart_formdata

let%expect_test "name" =
  let lb = Lexing.from_string {|boundary=gc0p4Jq0M2Yt08j34c0p |} in
  Lexer.lex_boundary lb
  |> Lexer.sexp_of_result
  |> Sexplib0.Sexp.to_string_hum
  |> print_endline;
  [%expect {| (Ok gc0p4Jq0M2Yt08j34c0p) |}]
