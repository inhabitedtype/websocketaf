open Core
open Async

let connection_handler
  : ([< Socket.Address.t] as 'a) -> ([`Active], 'a) Socket.t -> unit Deferred.t
  =
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
      Log.Global.error "EOF\n%!"
    in
    { Websocketaf.Server_connection.frame
    ; eof
    }
  in

  let error_handler wsd (`Exn exn) =
    let message = Exn.to_string exn in
    let payload = Bytes.of_string message in
    Websocketaf.Wsd.send_bytes wsd ~kind:`Text payload ~off:0
      ~len:(Bytes.length payload);
    Websocketaf.Wsd.close wsd
  in
  let http_error_handler _client_address ?request:_ error handle =
    let message =
      match error with
      | `Exn exn -> Exn.to_string exn
      | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
    in
    let body = handle Headers.empty in
    Body.write_string body message;
    Body.close_writer body
  in
  let request_handler _addr reqd =
    Websocketaf_async.Server.upgrade_connection
      ~reqd
      ~error_handler
      websocket_handler
    |> Deferred.don't_wait_for

  in
  Httpaf_async.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler:http_error_handler

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Tcp.(Server.create_sock ~on_handler_error:`Raise
      ~backlog:10_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    connection_handler
  >>= fun _server ->
  Log.Global.printf "Listening on port %i and echoing websocket messages.\n%!" port;
  Deferred.never ()

let () =
  Command.async_spec
    ~summary:"Echoes websocket messages. Runs forever."
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
      +>
      flag "-a" (optional_with_default 1 int)
        ~doc:"int Maximum accepts per batch"
    ) main
  |> Command.run
