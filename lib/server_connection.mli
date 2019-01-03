module IOVec = Httpaf.IOVec

type 'handle t

type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

val create
  : sha1 : (string -> string)
  -> websocket_handler : (Wsd.t -> input_handlers)
  -> 'handle t

val next_read_operation  : _ t -> [ `Read | `Yield | `Close ]
val next_write_operation : _ t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

val read : _ t -> Bigstring.t -> off:int -> len:int -> int
val read_eof : _ t -> Bigstring.t -> off:int -> len:int -> int
val report_write_result : _ t -> [`Ok of int | `Closed ] -> unit

val yield_reader : _ t -> (unit -> unit) -> unit
val yield_writer : _ t -> (unit -> unit) -> unit

val close : _ t -> unit
