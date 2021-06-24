open Httpaf
open Httpaf_lwt_unix
open Lwt.Infix

let request_handler (_ : Unix.sockaddr) reqd =
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  Body.close_reader request_body ;
  Lwt.async (fun () ->
      Lwt.pause ()
      >>= fun () ->
      ( match (request.meth, request.target) with
      | `GET, "/" ->
          let page =
            {|<!DOCTYPE html>
              <html>
              <head>
                <title>Upload multiple files</title>
              </head>
              <body>
                <div>
                  <form action="http://localhost:1234" method="post" enctype="multipart/form-data">
                    <input type="file" name="name" multiple><br>
                    <br>
                    After uploading multiple files, click Submit.<br>
                    <input type="submit" value="Submit">
                  </form>
                <div>
              </body>
              </html>|}
          in
          let page =
            Bigstringaf.of_string ~off:0 ~len:(String.length page) page in
          let headers =
            Headers.of_list
              [ ("content-length", Int.to_string (Bigstringaf.length page))
              ; ("content-type", "text/html") ] in
          Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) page
      | `POST, "/upload" -> ()
      | `GET, "/exit" -> exit 0
      | _ ->
          Reqd.respond_with_string reqd
            (Response.create `Not_found)
            "Route not found" ) ;
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
  let open Lwt.Infix in
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
