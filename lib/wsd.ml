module IOVec = Httpaf.IOVec

type mode =
  [ `Client of unit -> int32
  | `Server
  ]

type t =
  { faraday : Faraday.t
  ; mode : mode
  ; mutable when_ready_to_write : unit -> unit
  }

let default_ready_to_write = Sys.opaque_identity (fun () -> ())

let create mode =
  { faraday = Faraday.create 0x1000
  ; mode
  ; when_ready_to_write = default_ready_to_write;
  }

let mask t =
  match t.mode with
  | `Client m -> Some (m ())
  | `Server -> None

let ready_to_write t =
  let callback = t.when_ready_to_write in
  t.when_ready_to_write <- default_ready_to_write;
  callback ()

let schedule t ~kind payload ~off ~len =
  let mask = mask t in
  Websocket.Frame.schedule_serialize t.faraday ?mask ~is_fin:true ~opcode:(kind :> Websocket.Opcode.t) ~payload ~off ~len;
  ready_to_write t

let send_bytes t ~kind payload ~off ~len =
  let mask = mask t in
  Websocket.Frame.schedule_serialize_bytes t.faraday ?mask ~is_fin:true ~opcode:(kind :> Websocket.Opcode.t) ~payload ~off ~len;
  ready_to_write t

let send_ping t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Ping;
  ready_to_write t

let send_pong t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Pong;
  ready_to_write t

let flushed t f = Faraday.flush t.faraday f

let close t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Connection_close;
  Faraday.close t.faraday;
  ready_to_write t

let next t =
  match Faraday.operation t.faraday with
  | `Close         -> `Close 0 (* XXX(andreas): should track unwritten bytes *)
  | `Yield         -> `Yield
  | `Writev iovecs -> `Write iovecs

let report_result t result =
  match result with
  | `Closed -> close t
  | `Ok len -> Faraday.shift t.faraday len

let is_closed t =
  Faraday.is_closed t.faraday

let when_ready_to_write t callback =
  if not (t.when_ready_to_write == default_ready_to_write)
  then failwith "Wsd.when_ready_to_write: only one callback can be registered at a time"
  else if is_closed t
  then callback ()
  else t.when_ready_to_write <- callback
