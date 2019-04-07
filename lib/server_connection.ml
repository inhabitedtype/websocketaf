module IOVec = Httpaf.IOVec

type 'handle state =
  | Uninitialized
  | Handshake of 'handle Server_handshake.t
  | Websocket of Server_websocket.t

type 'handle t = 'handle state ref

type input_handlers = Server_websocket.input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

let passes_scrutiny _headers =
  true (* XXX(andreas): missing! *)

let create ~sha1 ~websocket_handler =
  let t = ref Uninitialized in
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
      (* XXX(andreas): this is a hacky workaround for a missing flush hook *)
      let body = Httpaf.Reqd.respond_with_streaming reqd response in
      Httpaf.Body.write_string body " ";
      Httpaf.Body.flush body (fun () ->
        t := Websocket (Server_websocket.create ~websocket_handler);
        Httpaf.Body.close_writer body
      )
    end
  in
  let handshake =
    Server_handshake.create
      ~request_handler
  in
  t := Handshake handshake;
  t
;;

let next_read_operation t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.next_read_operation handshake
  | Websocket websocket -> (Server_websocket.next_read_operation websocket :> [ `Read | `Yield | `Close ])
;;

let read t bs ~off ~len =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.read handshake bs ~off ~len
  | Websocket websocket -> Server_websocket.read websocket bs ~off ~len
;;

let read_eof t bs ~off ~len =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.read_eof handshake bs ~off ~len
  | Websocket websocket -> Server_websocket.read_eof websocket bs ~off ~len
;;

let yield_reader t f =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.yield_reader handshake f
  | Websocket _         -> assert false
;;

let next_write_operation t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.next_write_operation handshake
  | Websocket websocket -> Server_websocket.next_write_operation websocket
;;

let report_write_result t result =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.report_write_result handshake result
  | Websocket websocket -> Server_websocket.report_write_result websocket result
;;

let yield_writer t f =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.yield_writer handshake f
  | Websocket websocket -> Server_websocket.yield_writer websocket f
;;

let close t =
  match !t with
  | Uninitialized       -> assert false
  | Handshake handshake -> Server_handshake.close handshake
  | Websocket websocket -> Server_websocket.close websocket
;;
