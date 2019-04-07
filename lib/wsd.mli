module IOVec = Httpaf.IOVec

type mode =
  [ `Client of unit -> int32
  | `Server
  ]

type t 

val create
  : mode
  -> t

val schedule
  :  t
  -> kind:[ `Text | `Binary ]
  -> Bigstringaf.t
  -> off:int
  -> len:int
  -> unit

val send_bytes
  :  t
  -> kind:[ `Text | `Binary ]
  -> Bytes.t
  -> off:int
  -> len:int
  -> unit

val send_ping : t -> unit
val send_pong : t -> unit

val flushed : t -> (unit -> unit) -> unit
val close   : t -> unit

val next : t -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]
val report_result : t -> [`Ok of int | `Closed ] -> unit

val is_closed : t -> bool

val when_ready_to_write : t -> (unit -> unit) -> unit
