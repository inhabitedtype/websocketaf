module IOVec = Httpaf.IOVec

type t

type error =
  [ Httpaf.Client_connection.error
  | `Handshake_failure of Httpaf.Response.t * [`read] Httpaf.Body.t ]

type input_handlers = Client_websocket.input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                            -> unit }

val create
  :  nonce             : string
  -> host              : string
  -> port              : int
  -> resource          : string
  -> sha1              : (string -> string)
  -> error_handler     : (error -> unit)
  -> websocket_handler : (Wsd.t -> input_handlers)
  -> t

val next_read_operation  : t -> [ `Read | `Close ]
val next_write_operation : t -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

val read : t -> Bigstringaf.t -> off:int -> len:int -> int
val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int

val report_write_result : t -> [`Ok of int | `Closed ] -> unit

val yield_writer : t -> (unit -> unit) -> unit

val close : t -> unit
