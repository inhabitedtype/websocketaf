module IOVec = Httpaf.IOVec

type 'handle t =
  { connection : 'handle Httpaf.Server_connection.t
  }

let create
    ~request_handler
  =
  let connection =
    Httpaf.Server_connection.create
      request_handler
  in
  { connection }
;;

let next_read_operation t =
  Httpaf.Server_connection.next_read_operation t.connection

let next_write_operation t =
  Httpaf.Server_connection.next_write_operation t.connection

let read t =
  Httpaf.Server_connection.read t.connection

let read_eof t =
  Httpaf.Server_connection.read_eof t.connection

let report_write_result t =
  Httpaf.Server_connection.report_write_result t.connection

let yield_reader t =
  Httpaf.Server_connection.yield_reader t.connection

let yield_writer t =
  Httpaf.Server_connection.yield_writer t.connection

let close t =
  Httpaf.Server_connection.shutdown t.connection
