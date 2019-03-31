module IOVec = Httpaf.IOVec
include Httpaf.Client_connection

let create 
    ~nonce 
    ~host 
    ~port 
    ~resource
    ~error_handler
    ~response_handler 
  =
  let nonce = Base64.encode_exn nonce in
  let headers =
    [ "upgrade"              , "websocket"
    ; "connection"           , "upgrade"
    ; "host"                 , String.concat ":" [ host; string_of_int port ]
    ; "sec-websocket-version", "13" 
    ; "sec-websocket-key"    , nonce
    ] |> Httpaf.Headers.of_list
  in
  let request_body, t =
    Httpaf.Client_connection.request
      (Httpaf.Request.create ~headers `GET resource)
      ~error_handler
      ~response_handler
  in
  Httpaf.Body.close_writer request_body;
  t
;;
