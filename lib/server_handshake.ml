module IOVec = Httpaf.IOVec

type 'handle t =
  { conn: 'handle Httpaf.Server_connection.t
  ; mutable pending_bytes: [ `Ok of int | `Error ]
  ; wakeup_reader : (unit -> unit) list ref
  }

let create ~request_handler ~fd =
  { conn = Httpaf.Server_connection.create ~fd request_handler
  ; pending_bytes = `Ok 0
  ; wakeup_reader = ref []
  }

let next_read_operation t =
  Httpaf.Server_connection.next_read_operation t.conn

let next_write_operation t =
  match Httpaf.Server_connection.next_write_operation t.conn with
  | `Write iovecs as op ->
    begin match t.pending_bytes with
    | `Ok pending_bytes ->
      let lenv = Httpaf.IOVec.lengthv iovecs in
      t.pending_bytes <- `Ok (pending_bytes + lenv);
    | `Error -> ()
    end;
    op
  | op -> op

let read t =
  Httpaf.Server_connection.read t.conn

let read_eof t =
  Httpaf.Server_connection.read_eof t.conn

let report_write_result t result =
  Httpaf.Server_connection.report_write_result t.conn result;
  begin match result with
  | `Ok bytes_written ->
    begin match t.pending_bytes with
    | `Ok pending_bytes ->
      let pending_bytes' = pending_bytes - bytes_written in
      t.pending_bytes <- `Ok pending_bytes';
      `Ok pending_bytes'
    | `Error -> `Error
    end;
  | `Closed -> `Closed
  end

let reset_handshake t =
  t.pending_bytes <- `Ok 0

let report_handshake_failure t =
  t.pending_bytes <- `Error

let yield_reader t =
  Httpaf.Server_connection.yield_reader t.conn

let yield_writer t =
  Httpaf.Server_connection.yield_writer t.conn

let close t =
  Httpaf.Server_connection.shutdown t.conn
