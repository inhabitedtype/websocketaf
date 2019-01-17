let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in

  let websocket_handler _ wsd =
    let frame ~opcode ~is_fin:_ bs ~off ~len =
      match opcode with
      | `Continuation
      | `Text
      | `Binary ->
        Websocketaf.Wsd.schedule wsd bs ~kind:`Text ~off ~len
      | `Connection_close ->
        Websocketaf.Wsd.close wsd
      | `Ping ->
        Websocketaf.Wsd.send_ping wsd
      | `Pong
      | `Other _ ->
        ()
    in
    let eof () = ()
    in
    { Websocketaf.Server_connection.frame
    ; eof
    }
  in

  let error_handler _client_address ?request:_ error start_response =
    let response_body = start_response Headers.empty in

    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn);
      Body.write_string response_body "\n";

    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;
  in

  Websocketaf_lwt.Server.create_connection_handler
    ?config:None
    ~websocket_handler
    ~error_handler



let () =
  let open Lwt.Infix in

  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes websocket messages. Runs forever.";

  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in

  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address connection_handler
    >>= fun _server ->
      Printf.printf "Listening on port %i and echoing websocket messages.\n" !port;
      flush stdout;
      Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
