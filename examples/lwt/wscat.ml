open Lwt.Infix

let websocket_handler wsd =
  let rec input_loop wsd () =
    Lwt_io.(read_line stdin) >>= fun line ->
    let payload = Bytes.of_string line in
    Websocketaf.Wsd.send_bytes wsd ~kind:`Text payload ~off:0 ~len:(Bytes.length payload);
    input_loop wsd ()
  in
  Lwt.async (input_loop wsd);
  let frame ~opcode:_ ~is_fin:_ bs ~off ~len =
    let payload = Bytes.create len in
    Lwt_bytes.blit_to_bytes
      bs off
      payload 0
      len;
    Printf.printf "%s\n" (Bytes.unsafe_to_string payload);
    flush stdout
  in
  let eof () =
    Printf.printf "[EOF]\n"
  in
  { Websocketaf.Client_connection.frame
  ; eof
  }

let error_handler = function
  | `Handshake_failure (rsp, _body) ->
    Format.printf "Handshake failure: %a" Httpaf.Response.pp_hum rsp
  | _ -> assert false

let () =
  let host = ref None in
  let port = ref 80 in

  Arg.parse
    ["-p", Set_int port, " Port number (80 by default)"]
    (fun host_argument -> host := Some host_argument)
    "wscat.exe [-p N] HOST";

  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in

  Lwt_main.run begin
    Lwt_unix.getaddrinfo host (string_of_int !port) [Unix.(AI_FAMILY PF_INET)]
    >>= fun addresses ->

    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr
    >>= fun () ->

    let nonce = "0123456789ABCDEF" in
    let resource = "/" in
    let port = !port in
    Websocketaf_lwt.Client.connect
      socket
      ~nonce
      ~host
      ~port
      ~resource
      ~error_handler
      ~websocket_handler
  end
