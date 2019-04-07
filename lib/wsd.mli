type t 

val schedule
  :  ?mask:int32
  -> t
  -> kind:[ `Text | `Binary ]
  -> Bigstringaf.t
  -> off:int
  -> len:int
  -> unit

val send_bytes
  :  ?mask:int32
  -> t
  -> kind:[ `Text | `Binary ]
  -> Bytes.t
  -> off:int
  -> len:int
  -> unit

val send_ping : t -> unit
val send_pong : t -> unit

val flushed : t -> (unit -> unit) -> unit
val close   : t -> unit
