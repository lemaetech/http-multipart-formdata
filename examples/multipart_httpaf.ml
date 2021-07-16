open Httpaf
open Httpaf_lwt_unix
open Lwt.Infix
module Multipart = Http_multipart_formdata.Make (Reparse_lwt.Stream)

let upload_page =
  {|<!DOCTYPE html>
    <html>
    <head>
      <title>Upload multiple files</title>
      <style>
        .main {
          margin: auto;
          padding: 20px;
          font-family: Arial, sans-serif;
          width: 600px;
          border: 1px solid gray;
        }
      </style>
    </head>
    <body>
      <div class="main">
        <form action="http://localhost:8080/upload" method="post" enctype="multipart/form-data">
          <label>Select files to upload:</label>
          <input type="file" name="name" multiple><br>
          <input type="submit" value="Upload">
        </form>
      </div>
    </body>
    </html>|}

type parse_result = ((Multipart.part_header * string) list, string) result
[@@deriving show]

let handle_upload content_type req_body_stream =
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
  lift (Http_multipart_formdata.parse_boundary ~content_type)
  >>= fun boundary ->
  let input = Reparse_lwt.Stream.create_input req_body_stream in
  let reader = Multipart.reader ~read_body_len:10 boundary input in
  read_parts reader (Queue.create ())

let request_handler (_ : Unix.sockaddr) reqd =
  let req_body_stream, push = Lwt_stream.create () in
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  Body.schedule_read request_body
    ~on_eof:(fun () -> push None)
    ~on_read:(fun bs ~off ~len ->
      for i = 0 to len - off - 1 do
        let c = Bigstringaf.get bs i in
        push (Some c)
      done ) ;
  Body.close_reader request_body ;
  Lwt.async (fun () ->
      match (request.meth, request.target) with
      | `GET, "/" ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length upload_page))
              ; ("content-type", "text/html") ]
          in
          Reqd.respond_with_string reqd
            (Response.create ~headers `OK)
            upload_page ;
          Lwt.return_unit
      | `POST, "/upload" ->
          let content_type = Headers.get_exn request.headers "content-type" in
          handle_upload content_type req_body_stream
          >>= fun parts ->
          Lwt.return (show_parse_result parts)
          >|= fun s ->
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (String.length s))
              ; ("content-type", "text/plain") ]
          in
          Reqd.respond_with_string reqd (Response.create ~headers `OK) s
      | `GET, "/exit" -> Lwt.return_unit
      | _ ->
          Reqd.respond_with_string reqd
            (Response.create `Not_found)
            "Route not found" ;
          Lwt.return_unit )

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  ( match error with
  | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn) ;
      Body.write_string response_body "\n"
  | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error) ) ;
  Body.close_writer response_body

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 listen_address
        (Server.create_connection_handler ~request_handler ~error_handler)
      >>= fun _server -> Lwt.return_unit ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = ref 8080 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (8080 by default)")]
    ignore "Responds to requests with a fixed string for benchmarking purposes" ;
  main !port
