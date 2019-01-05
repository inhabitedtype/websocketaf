open Core
open Async

let sha1 s =
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> B64.encode ~pad:true

(** XXX(seliopou): Replace Angstrom.Buffered with a module like this, while
    also supporting growing the buffer. Clients can use this to buffer and the
    use the unbuffered interface for actually running the parser. *)
module Buffer = struct
  type t =
    { buffer      : Bigstring.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Bigstring.create size in
    { buffer; off = 0; len = 0 }
  ;;

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Bigstring.blit ~src:t.buffer ~src_pos:t.off ~dst:t.buffer ~dst_pos:0 ~len:t.len;
      t.off <- 0;
    end
  ;;

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n
  ;;

  let put t ~f =
    compress t;
    let n = f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len) in
    t.len <- t.len + n;
    n
  ;;
end

let read fd buffer =
  let badfd fd = failwithf "read got back fd: %s" (Fd.to_string fd) () in
  let rec finish fd buffer result =
    let open Unix.Error in
    match result with
    | `Already_closed | `Ok 0 -> return `Eof
    | `Ok n                   -> return (`Ok n)
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      begin Fd.ready_to fd `Read
      >>= function
        | `Bad_fd -> badfd fd
        | `Closed -> return `Eof
        | `Ready  -> go fd buffer
      end
    | `Error (Unix.Unix_error (EBADF, _, _)) ->
      badfd fd
    | `Error exn ->
      Deferred.don't_wait_for (Fd.close fd);
      raise exn
  and go fd buffer  =
    if Fd.supports_nonblock fd then
      finish fd buffer
        (Fd.syscall fd ~nonblocking:true
          (fun file_descr ->
            Buffer.put buffer ~f:(fun bigstring ~off ~len ->
              Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
                (Bigstring.read_assume_fd_is_nonblocking file_descr bigstring ~pos:off ~len))))
    else
      Fd.syscall_in_thread fd ~name:"read"
        (fun file_descr ->
          Buffer.put buffer ~f:(fun bigstring ~off ~len ->
            Bigstring.read file_descr bigstring ~pos:off ~len))
      >>= fun result -> finish fd buffer result
  in
  go fd buffer

module Server = struct
  module Server_connection = Websocketaf.Server_connection

  let start_read_write_loops ~socket connection =
    let fd = Socket.fd socket in
    let read_complete = Ivar.create () in
    let buffer = Buffer.create 0x1000 in
    let rec reader_thread () =
      match Server_connection.next_read_operation connection with
      | `Read ->
        read fd buffer
        >>> begin function
          | `Eof  ->
            Buffer.get buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            reader_thread ()
          | `Ok _ ->
            Buffer.get buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read connection bigstring ~off ~len)
            |> ignore;
            reader_thread ()
        end

      | `Yield  ->
        Server_connection.yield_reader connection reader_thread

      | `Close ->

        Ivar.fill read_complete ();
        if not (Fd.is_closed fd)
        then Socket.shutdown socket `Receive
    in

    let writev = Faraday_async.writev_of_fd fd in
    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Server_connection.next_write_operation connection with
      | `Write iovecs ->
        writev iovecs >>> fun result ->
          Server_connection.report_write_result connection result;
          writer_thread ()

      | `Yield ->
        Server_connection.yield_writer connection writer_thread;

      | `Close _ ->
        Ivar.fill write_complete ();
        if not (Fd.is_closed fd)
        then Socket.shutdown socket `Send
    in
    let conn_monitor = Monitor.create () in
    Scheduler.within ~monitor:conn_monitor reader_thread;
    Scheduler.within ~monitor:conn_monitor writer_thread;
    Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
      Server_connection.close connection;
      Log.Global.error "%s\n%!" (Exn.to_string exn);
      if not (Fd.is_closed fd)
      then don't_wait_for (Fd.close fd));
    (* The Tcp module will close the file descriptor once this becomes determined. *)
    Deferred.all_unit
      [ Ivar.read read_complete
      ; Ivar.read write_complete ]

  let create_connection_handler
    ?config:(_ : Httpaf.Server_connection.Config.t option)
    ~websocket_handler
    ~error_handler:_ =
    fun (client_addr : [< Socket.Address.t]) socket ->
      let websocket_handler = websocket_handler client_addr in
      let conn = Server_connection.create ~sha1 ~fd:socket websocket_handler in
      start_read_write_loops ~socket conn

  let upgrade_connection ?config:_ ?headers ~reqd ~error_handler websocket_handler =
    match
      Server_connection.upgrade
        ~sha1
        ~reqd
        ?headers
        ~error_handler
        websocket_handler
    with
    | Ok connection ->
      let socket = Httpaf.Reqd.descriptor reqd in
      start_read_write_loops ~socket connection >>| fun x -> Ok x
    | Error _ as err -> Deferred.return err
end

module Client = struct
  module Client_connection = Websocketaf.Client_connection

  let connect socket ~nonce ~host ~port ~resource ~error_handler ~websocket_handler =
    let fd     = Socket.fd socket in
    let writev = Faraday_async.writev_of_fd fd in
    let conn =
      Client_connection.create ~nonce ~host ~port ~resource ~sha1 ~error_handler ~websocket_handler
    in
    let read_complete = Ivar.create () in
    let buffer = Buffer.create 0x1000 in
    let rec reader_thread () =
      match Client_connection.next_read_operation conn with
      | `Read ->
        read fd buffer
          >>> begin function
            | `Eof  ->
              Buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Client_connection.read_eof conn bigstring ~off ~len)
              |> ignore;
              reader_thread ()
            | `Ok _ ->
              Buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Client_connection.read conn bigstring ~off ~len)
              |> ignore;
              reader_thread ()
          end
      | `Close ->
        Ivar.fill read_complete ();
        if not (Fd.is_closed fd)
        then Socket.shutdown socket `Receive
    in

    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Client_connection.next_write_operation conn with
      | `Write iovecs ->
        writev iovecs >>> fun result ->
          Client_connection.report_write_result conn result;
          writer_thread ()
      | `Yield ->
        Client_connection.yield_writer conn writer_thread;
      | `Close _ ->
        Ivar.fill write_complete ();
    in
    let conn_monitor = Monitor.create () in
    Scheduler.within ~monitor:conn_monitor reader_thread;
    Scheduler.within ~monitor:conn_monitor writer_thread;
    Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
      Client_connection.close conn;
      Log.Global.error "%s" (Exn.to_string exn);
      if not (Fd.is_closed fd)
      then don't_wait_for (Fd.close fd));
     Deferred.all_unit
      [ Ivar.read read_complete
      ; Ivar.read write_complete ]
      >>| fun () ->
        if not (Fd.is_closed fd)
        then don't_wait_for (Fd.close fd)
end
