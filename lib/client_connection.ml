module IOVec = Httpaf.IOVec

type state =
  | Uninitialized
  | Handshake of Client_handshake.t
  | Websocket of Client_websocket.t

type t = state ref

type error =
  [ Httpaf.Client_connection.error
  | `Handshake_failure of Httpaf.Response.t * [`read] Httpaf.Body.t ]

type input_handlers = Client_websocket.input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

let passes_scrutiny ~accept headers =
  let upgrade              = Httpaf.Headers.get headers "upgrade"    in
  let connection           = Httpaf.Headers.get headers "connection" in
  let sec_websocket_accept = Httpaf.Headers.get headers "sec-websocket-accept" in
  sec_websocket_accept = Some accept
  && (match upgrade with
     | None         -> false
     | Some upgrade -> String.lowercase_ascii upgrade = "websocket")
  && (match connection with
     | None            -> false
     | Some connection -> String.lowercase_ascii connection = "upgrade")
;;

let handshake_exn t =
  match !t with
  | Handshake handshake -> handshake
  | Uninitialized
  | Websocket _ -> assert false

let create
    ~nonce
    ~host
    ~port
    ~resource
    ~sha1
    ~error_handler
    ~websocket_handler
  =
  let t = ref Uninitialized in
  let nonce = B64.encode nonce in
  let response_handler response response_body =
    let accept = sha1 (nonce ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") in
    match response.Httpaf.Response.status with
    | `Switching_protocols when passes_scrutiny ~accept response.headers ->
      Httpaf.Body.close_reader response_body;
      let handshake = handshake_exn t in
      t := Websocket (Client_websocket.create ~websocket_handler);
      Client_handshake.close handshake
    | _                    ->
      error_handler (`Handshake_failure(response, response_body))
  in
  let handshake =
    let error_handler = (error_handler :> Httpaf.Client_connection.error_handler) in
    Client_handshake.create
      ~nonce
      ~host
      ~port
      ~resource
      ~error_handler
      ~response_handler
  in
  t := Handshake handshake;
  t
;;

let next_read_operation t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.next_read_operation handshake
  | Websocket websocket -> Client_websocket.next_read_operation websocket
;;

let read t bs ~off ~len =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.read handshake bs ~off ~len
  | Websocket websocket -> Client_websocket.read websocket bs ~off ~len
;;

let read_eof t bs ~off ~len =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.read handshake bs ~off ~len
  | Websocket websocket -> Client_websocket.read_eof websocket bs ~off ~len
;;

let next_write_operation t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.next_write_operation handshake
  | Websocket websocket -> Client_websocket.next_write_operation websocket
;;

let report_write_result t result =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.report_write_result handshake result
  | Websocket websocket -> Client_websocket.report_write_result websocket result
;;

let yield_writer t f =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.yield_writer handshake f
  | Websocket websocket -> Client_websocket.yield_writer websocket f
;;

let close t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Client_handshake.close handshake
  | Websocket websocket -> Client_websocket.close websocket
;;
