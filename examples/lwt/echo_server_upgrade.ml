open Lwt.Infix

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in

  let websocket_handler wsd =
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
    let eof () =
      Printf.eprintf "EOF\n%!"
    in
    { Websocketaf.Server_connection.frame
    ; eof
    }
  in

  let error_handler wsd (`Exn exn) =
    let message = Printexc.to_string exn in
    let payload = Bytes.of_string message in
    Websocketaf.Wsd.send_bytes wsd ~kind:`Text payload ~off:0
      ~len:(Bytes.length payload);
    Websocketaf.Wsd.close wsd
  in
  let http_error_handler _client_address ?request:_ error handle =
    let message =
      match error with
      | `Exn exn -> Printexc.to_string exn
      | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
    in
    let body = handle Headers.empty in
    Body.write_string body message;
    Body.close_writer body
  in
  let request_handler _addr reqd =
    (Websocketaf_lwt.Server.upgrade_connection
      ~reqd
      ~error_handler
      websocket_handler
    >|= function
    | Ok () -> ()
    | Error err_str ->
      let response = Response.create
        ~headers:(Httpaf.Headers.of_list ["Connection", "close"])
        `Bad_request
      in
      Reqd.respond_with_string reqd response err_str)
    |> Lwt.ignore_result

  in
  Httpaf_lwt.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler:http_error_handler


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
      Printf.printf "Listening on port %i and echoing websocket messages.\n%!" !port;
      Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

