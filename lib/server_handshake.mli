module IOVec = Httpaf.IOVec

type 'handle t

val create
  :  request_handler : 'handle Httpaf.Server_connection.request_handler
  -> 'handle t

val next_read_operation  : _ t -> [ `Read | `Close | `Yield ]
val next_write_operation : _ t -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

val read : _ t -> Bigstringaf.t -> off:int -> len:int -> int
val read_eof : _ t -> Bigstringaf.t -> off:int -> len:int -> int
val report_write_result : _ t -> [`Ok of int | `Closed ] -> unit

val yield_reader : _ t -> (unit -> unit) -> unit
val yield_writer : _ t -> (unit -> unit) -> unit

val close : _ t -> unit
