module IOVec = Httpaf.IOVec

type t =
  { connection : Httpaf.Client_connection.t
  ; body       : [`write] Httpaf.Body.t }

let create
    ~nonce
    ~host
    ~port
    ~resource
    ~error_handler
    ~response_handler
  =
  let headers =
    [ "upgrade"              , "websocket"
    ; "connection"           , "upgrade"
    ; "host"                 , String.concat ":" [ host; string_of_int port ]
    ; "sec-websocket-version", "13"
    ; "sec-websocket-key"    , nonce
    ] |> Httpaf.Headers.of_list
  in
  let body, connection =
    Httpaf.Client_connection.request
      (Httpaf.Request.create ~headers `GET resource)
      ~error_handler
      ~response_handler
  in
  { connection
  ; body
  }
;;

let next_read_operation t =
  Httpaf.Client_connection.next_read_operation t.connection

let next_write_operation t =
  Httpaf.Client_connection.next_write_operation t.connection

let read t =
  Httpaf.Client_connection.read t.connection

let report_write_result t =
  Httpaf.Client_connection.report_write_result t.connection

let yield_writer t =
  Httpaf.Client_connection.yield_writer t.connection

let close t =
  Httpaf.Body.close_writer t.body
