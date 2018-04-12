type t = Faraday.t

let schedule ?mask t ~kind payload ~off ~len =
  Websocket.Frame.schedule_serialize ?mask t ~is_fin:true ~opcode:kind ~payload ~off ~len

let send_bytes ?mask t ~kind payload ~off ~len =
  Websocket.Frame.schedule_serialize_bytes ?mask t ~is_fin:true ~opcode:kind ~payload ~off ~len

let send_ping t =
  Websocket.Frame.serialize_control t ~opcode:`Ping

let send_pong t =
  Websocket.Frame.serialize_control t ~opcode:`Pong

let flushed t f = Faraday.flush t f

let close t =
  Websocket.Frame.serialize_control t ~opcode:`Connection_close;
  Faraday.close t
;;
