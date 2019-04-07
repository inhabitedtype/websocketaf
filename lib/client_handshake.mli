module IOVec = Httpaf.IOVec

type t

val create
  :  nonce            : string
  -> host             : string
  -> port             : int
  -> resource         : string
  -> error_handler    : Httpaf.Client_connection.error_handler
  -> response_handler : Httpaf.Client_connection.response_handler
  -> t

val next_read_operation  : t -> [ `Read | `Close ]
val next_write_operation : t -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

val read : t -> Bigstringaf.t -> off:int -> len:int -> int
val report_write_result : t -> [`Ok of int | `Closed ] -> unit

val yield_writer : t -> (unit -> unit) -> unit

val close : t -> unit
