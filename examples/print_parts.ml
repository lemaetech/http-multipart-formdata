module Multipart = Http_multipart_formdata.Make (Reparse_lwt.Stream)

type parse_result = ((Multipart.part_header * string) list, string) result
[@@deriving show]

let () =
  let body =
    String.concat "\r\n"
      [ {||}
      ; {| this is a preamble text.  |}
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
      ; {|<!DOCTYPE html><title>Content of a.html.</title><div>thiasdasdf asdfiasdf  asdf asdf as df asdf asdf as df asdf asd fa sdf asd fas df asdf as df asd fas df asdf as df asdfas df asd fa sdf as dfa sdf asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfsadfsadfasdfasdfasdfasdfasdfsadfasdfasdfasdfasdfasdfsadfasdfasdfasdfasdfasdfasdfasdfasdfasdf
asdfasdfasdfasdfasdfasdf|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"; param1=value1; param2=value2|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {|-----------------------------735323031399963166993862150--|} ]
  in
  let open Lwt_result in
  let rec read_parts reader parts =
    ok (Multipart.read_part reader)
    >>= function
    | `End -> Lwt.return (Ok (Queue.to_seq parts |> List.of_seq))
    | `Header header ->
        read_body reader []
        >>= fun body ->
        let body = List.rev body |> Cstruct.concat |> Cstruct.to_string in
        Queue.push (header, body) parts ;
        read_parts reader parts
    | `Error e -> Lwt.return (Error e)
    | _ -> assert false
  and read_body reader buffers =
    ok (Multipart.read_part reader)
    >>= function
    | `Body_end -> return buffers
    | `Body buf -> read_body reader (buf :: buffers)
    | `Error e -> fail e
    | _ -> assert false
  in
  let content_type =
    {|multipart/form-data; boundary=---------------------------735323031399963166993862150|}
  in
  lift (Http_multipart_formdata.parse_boundary ~content_type)
  >>= (fun boundary ->
        let input =
          Reparse_lwt.Stream.create_input (Lwt_stream.of_string body)
        in
        let reader = Multipart.reader ~read_body_len:10 boundary input in
        read_parts reader (Queue.create ()) )
  |> Lwt_main.run
  |> pp_parse_result Format.std_formatter
