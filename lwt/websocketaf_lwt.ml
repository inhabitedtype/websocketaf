open Lwt.Infix

let sha1 s =
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> B64.encode ~pad:true

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
end = struct
  type t =
    { buffer      : Lwt_bytes.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Lwt_bytes.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Lwt_bytes.blit t.buffer t.off t.buffer 0 t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Lwt_bytes.length t.buffer - t.len)
    >>= fun n ->
    t.len <- t.len + n;
    Lwt.return n
end


let read fd buffer =
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
        Lwt_bytes.read fd bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
      Lwt.fail exn
    | exn ->
      Lwt.async (fun () ->
        Lwt_unix.close fd);
      Lwt.fail exn)

  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)

let shutdown socket command =
  try Lwt_unix.shutdown socket command
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()



module Server = struct
  module Server_connection = Websocketaf.Server_connection

  let start_read_write_loops ~socket connection =
    let read_buffer = Buffer.create 0x1000 in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Server_connection.next_read_operation connection with
        | `Read ->
          read socket read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Yield ->
          Server_connection.yield_reader connection read_loop;
          Lwt.return_unit

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
            (fun exn ->
              (* XXX(andreas): missing error reporting *)
              (* Server_connection.report_exn connection exn;*)
              Printexc.print_backtrace stdout;
              ignore(raise exn);
              Lwt.return_unit))
    in


    let writev = Faraday_lwt_unix.writev_of_fd socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Server_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Server_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Server_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            (* XXX(andreas): missing error reporting *)
            (*Server_connection.report_exn connection exn;*)
            Printexc.print_backtrace stdout;
            ignore(raise exn);
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();
    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit


  (* TODO: should this error handler be a websocket error handler or an HTTP
   * error handler?*)
  let create_connection_handler ?config:_ ~websocket_handler ~error_handler:_ =
    fun client_addr socket ->
      let websocket_handler = websocket_handler client_addr in
      let connection =
        Server_connection.create
          ~sha1
          ~fd:socket
          ~websocket_handler
      in
      start_read_write_loops ~socket connection


  let upgrade_connection ?config:_ ?headers ~reqd websocket_handler =
    let connection =
      Server_connection.upgrade
        ~sha1
        ~reqd
        ?headers
        websocket_handler
    in
    let socket = Httpaf.Reqd.descriptor reqd in
    start_read_write_loops ~socket connection
end



module Client = struct
  let connect socket ~nonce ~host ~port ~resource ~error_handler ~websocket_handler =
    let module Client_connection = Websocketaf.Client_connection in
    let connection =
      Client_connection.create ~nonce ~host ~port ~resource ~sha1 ~error_handler ~websocket_handler in

    let read_buffer = Buffer.create 0x1000 in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          read socket read_buffer >>= begin function
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            (*Client_connection.report_exn connection exn;*)
            Printexc.print_backtrace stdout;
            ignore(raise exn);
            Lwt.return_unit))
    in


    let writev = Faraday_lwt_unix.writev_of_fd socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        flush stdout;
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            (*Client_connection.report_exn connection exn;*)
            ignore(raise exn);
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit;
end
