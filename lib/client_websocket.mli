module IOVec = Httpaf.IOVec

type t

type input_handlers =
  { frame : Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                   -> unit }

val create 
  :  websocket_handler : (Wsd.t -> input_handlers)
  -> t

val next_read_operation  : t -> [ `Read | `Yield | `Close ]
val next_write_operation : t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

val read : t -> Bigstring.t -> off:int -> len:int -> int
val report_write_result : t -> [`Ok of int | `Closed ] -> unit

val yield_reader : t -> (unit -> unit) -> unit
val yield_writer : t -> (unit -> unit) -> unit

val close : t -> unit
