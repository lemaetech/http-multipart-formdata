open! Http_multipart_formdata

(* type string_result = (string, string) result [@@deriving show, ord] *)

(* let%expect_test "parse_boundary" = *)
(*   let content_type = *)
(* {|multipart/form-data;
   boundary=---------------------------735323031399963166993862150|} *)
(*   in *)
(*   parse_boundary ~content_type *)
(*   |> Lwt_main.run *)
(*   |> pp_string_result Format.std_formatter; *)
(* [%expect {| (Ok "---------------------------735323031399963166993862150") |}] *)

type parse_result = ((Part_header.t * string) list, string) result
[@@deriving show, ord]

let%expect_test "parse" =
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
    ; {|-----------------------------735323031399963166993862150--|}
    ]
    |> String.concat "\r\n"
    |> Lwt_stream.of_string
  in
  let parts = ref [] in
  let on_part header body_stream =
    Format.fprintf Format.std_formatter "\npart:%a\n\n%!" Part_header.pp header;
    let part_body = Buffer.create 0 in
    let rec part_reader () =
      Lwt.(
        Lwt_stream.get body_stream
        >>= function
        | Some c ->
          Format.fprintf Format.std_formatter "%c%!" c;
          Buffer.add_char part_body c;
          part_reader ()
        | None -> return (header, Buffer.contents part_body))
    in
    parts := part_reader () :: !parts
  in
  let content_type =
    {|multipart/form-data; boundary=---------------------------735323031399963166993862150|}
  in
  Lwt_result.(
    parse_boundary ~content_type
    >>= fun boundary -> parse ~boundary ~on_part body >|= fun () -> []
    (*@@ Lwt.all !parts *))
  |> Lwt_main.run
  |> pp_parse_result Format.std_formatter;
  [%expect {| (Ok []) |}]

(* let multi_values_suite = *)
(*   let content_type_header = *)
(* " multipart/form-data; \ *) (*
   boundary=---------------------------735323031399963166993862150" *)
(*   in *)
(*   let body = *)
(*     [ {||} *)
(*     ; {|-----------------------------735323031399963166993862150|} *)
(*     ; {|Content-Disposition: form-data; name="text1"|} *)
(*     ; {||} *)
(*     ; {|text default|} *)
(*     ; {|-----------------------------735323031399963166993862150|} *)
(*     ; {|Content-Disposition: form-data; name="text1"|} *)
(*     ; {||} *)
(*     ; {|aωb|} *)
(*     ; {|-----------------------------735323031399963166993862150|} *)
(*     ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|} *)
(*     ; {|Content-Type: text/plain|} *)
(*     ; {||} *)
(*     ; {|Content of a.txt.|} *)
(*     ; {||} *)
(*     ; {|-----------------------------735323031399963166993862150|} *)
(*     ; {|Content-Disposition: form-data; name="file1"; filename="a.html"|} *)
(*     ; {|Content-Type: text/html|} *)
(*     ; {||} *)
(*     ; {|<!DOCTYPE html><title>Content of a.html.</title>|} *)
(*     ; {||} *)
(*     ; {|-----------------------------735323031399963166993862150|} *)
(*     ; {|Content-Disposition: form-data; name="file1"; filename="binary"|} *)
(*     ; {|Content-Type: application/octet-stream|} *)
(*     ; {||} *)
(*     ; {|aωb|} *)
(*     ; {|-----------------------------735323031399963166993862150--|} *)
(*     ] *)
(*     |> String.concat "\r\n" *)
(*   in *)
(*   let mp = M.parse ~content_type_header ~body in *)
(*   let files_actual = M.Map.find "file1" mp in *)
(*   let files_expected = *)
(*     [ { M.Part.body = Bytes.of_string "\r\nContent of a.txt.\r\n\r\n" *)
(*       ; name = "file1" *)
(*       ; content_type = "text/plain" *)
(*       ; filename = Some "a.txt" *)
(*       ; parameters = M.Map.empty *)
(*       } *)
(*     ; { M.Part.body = *)
(*           Bytes.of_string *)
(*             "\r\n<!DOCTYPE html><title>Content of a.html.</title>\r\n\r\n" *)
(*       ; name = "file1" *)
(*       ; content_type = "text/html" *)
(*       ; filename = Some "a.html" *)
(*       ; parameters = M.Map.empty *)
(*       } *)
(*     ; { M.Part.body = Bytes.of_string "\r\naωb\r\n" *)
(*       ; name = "file1" *)
(*       ; content_type = "application/octet-stream" *)
(*       ; filename = Some "binary" *)
(*       ; parameters = M.Map.empty *)
(*       } *)
(*     ] *)
(*   in *)
(*   let text1_a = M.Map.find "text1" mp in *)
(*   let text1_e = *)
(*     [ { M.Part.body = Bytes.of_string "\r\ntext default\r\n" *)
(*       ; name = "text1" *)
(*       ; content_type = "text/plain" *)
(*       ; filename = None *)
(*       ; parameters = M.Map.empty *)
(*       } *)
(*     ; { M.Part.body = Bytes.of_string "\r\naωb\r\n" *)
(*       ; name = "text1" *)
(*       ; content_type = "text/plain" *)
(*       ; filename = None *)
(*       ; parameters = M.Map.empty *)
(*       } *)
(*     ] *)
(*   in *)
(*   [ ( "file1" *)
(*     , `Quick *)
(*     , fun () -> Alcotest.check parts "equal" files_actual files_expected ) *)
(* ; ("text1", `Quick, fun () -> Alcotest.check parts "equal" text1_a text1_e) *)
(* ] *)

(* let () = *)
(* Printexc.record_backtrace true; *)
(* Alcotest.run "M" *)
(*   [ ("Single Values", single_value_suite) *)
(*   ; ("Multi Values", multi_values_suite) *)
(*   ] *)
