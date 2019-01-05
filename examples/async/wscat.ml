open Core
open Async

let websocket_handler wsd =
  let rec input_loop wsd () =
    Reader.read_line (Lazy.force Reader.stdin) >>= function
    | `Ok line ->
      let payload = Bytes.of_string line in
      Websocketaf.Wsd.send_bytes wsd ~kind:`Text payload ~off:0 ~len:(Bytes.length payload);
      input_loop wsd ()
    | `Eof -> assert false
  in
  Deferred.don't_wait_for (input_loop wsd ());
  let frame ~opcode:_ ~is_fin:_ bs ~off ~len =
    let payload = Bigstring.to_bytes ~pos:off ~len bs in
    Log.Global.printf "%s\n%!" (Bytes.to_string payload);
  in
  let eof () =
    Log.Global.error "[EOF]\n%!"
  in
  { Websocketaf.Client_connection.frame
  ; eof
  }

let error_handler = function
  | `Handshake_failure (rsp, _body) ->
    Format.eprintf "Handshake failure: %a\n%!" Httpaf.Response.pp_hum rsp
  | _ -> assert false

let main port host () =
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    let nonce = "0123456789ABCDEF" in
    let resource = "/" in
    Websocketaf_async.Client.connect
      socket
      ~nonce
      ~host
      ~port
      ~resource
      ~error_handler
      ~websocket_handler
    (* Deferred.never finished *)

let () =
  Command.async_spec
    ~summary:"Start a websocket cat client"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 80 int)
        ~doc:"int destination port"
      +> anon ("Destination Host" %: string)
    ) main
  |> Command.run

