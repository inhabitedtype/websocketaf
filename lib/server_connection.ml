module IOVec = Httpaf.IOVec

type 'handle state =
  | Uninitialized
  | Handshake of 'handle Server_handshake.t
  | Websocket of Server_websocket.t

type input_handlers = Server_websocket.input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

type 'handle t =
  { mutable state: 'handle state
  ; websocket_handler: Wsd.t -> input_handlers
  }

let passes_scrutiny _headers =
  true (* XXX(andreas): missing! *)

let create ~sha1 ~fd ~websocket_handler =
  let t =
    { state = Uninitialized
    ; websocket_handler
    }
  in
  let request_handler reqd =
    let request = Httpaf.Reqd.request reqd in
    if passes_scrutiny request.headers then begin
      let key = Httpaf.Headers.get_exn request.headers "sec-websocket-key" in
      let accept = sha1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") in
      let headers = Httpaf.Headers.of_list [
        "upgrade",              "websocket";
        "connection",           "upgrade";
        "sec-websocket-accept", accept
      ] in
      let response = Httpaf.(Response.create ~headers `Switching_protocols) in
      let body = Httpaf.Reqd.respond_with_streaming
        reqd
        ~flush_headers_immediately:true
        response
      in
      match t.state with
      | Handshake handshake ->
          Server_handshake.reset_handshake handshake
      | _ -> ();
      Httpaf.Body.close_writer body
    end else begin
      let response = Httpaf.(Response.create
        ~headers:(Headers.of_list ["Connection", "close"])
        `Bad_request)
      in
      Httpaf.Reqd.respond_with_string reqd response "Didn't pass scrutiny";
      match t.state with
      | Handshake handshake ->
          Server_handshake.report_handshake_failure handshake
      | _ -> ();
    end
  in
  let handshake =
    Server_handshake.create
      ~fd
      ~request_handler
  in
  t.state <- Handshake handshake;
  t

let upgrade
  ~sha1
  ~reqd
  ?(headers=Httpaf.Headers.empty)
  websocket_handler =
  let request = Httpaf.Reqd.request reqd in
  if passes_scrutiny request.headers then begin
    let key = Httpaf.Headers.get_exn request.headers "sec-websocket-key" in
    let accept = sha1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") in
    let upgrade_headers = Httpaf.Headers.of_list [
      "Transfer-Encoding",    "chunked";
      "Upgrade",              "websocket";
      "Connection",           "upgrade";
      "Sec-Websocket-Accept", accept;
    ]
    in
    let headers = Httpaf.Headers.(add_list upgrade_headers (to_list headers)) in
    let response = Httpaf.(Response.create ~headers `Switching_protocols) in
    let _body = Httpaf.Reqd.respond_with_streaming
      reqd
      ~flush_headers_immediately:true
      response
    in ()
  end;
  { state = Websocket (Server_websocket.create ~websocket_handler)
  ; websocket_handler
  }

let next_read_operation t =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.next_read_operation handshake
  | Websocket websocket -> (Server_websocket.next_read_operation websocket :> [ `Read | `Yield | `Close ])
;;

let read t bs ~off ~len =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.read handshake bs ~off ~len
  | Websocket websocket -> Server_websocket.read websocket bs ~off ~len
;;

let read_eof t bs ~off ~len =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.read_eof handshake bs ~off ~len
  | Websocket websocket -> Server_websocket.read_eof websocket bs ~off ~len
;;

let yield_reader t f =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.yield_reader handshake f
  | Websocket _         -> assert false
;;

let next_write_operation t =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.next_write_operation handshake
  | Websocket websocket -> Server_websocket.next_write_operation websocket
;;

let report_write_result t result =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake ->
    begin match Server_handshake.report_write_result handshake result with
    | `Ok pending_bytes ->      if pending_bytes == 0 then
      let websocket_handler = t.websocket_handler in
      t.state <- Websocket (Server_websocket.create ~websocket_handler);
    | _ -> ()
    end
  | Websocket websocket -> Server_websocket.report_write_result websocket result
;;

let yield_writer t f =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.yield_writer handshake f
  | Websocket websocket -> Server_websocket.yield_writer websocket f
;;

let close t =
  match t.state with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.close handshake
  | Websocket websocket -> Server_websocket.close websocket
;;
