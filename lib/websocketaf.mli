(** Bigstring

    A block of memory allocated on the C heap. Bigstring payloads won't get
    relocated by the OCaml GC, making it safe to use in blocking system calls
    without holding the OCaml runtime lock. *)
module Bigstring : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** For compatiblity with other libraries, [Bigstring.t] is not abstract. *)

  val create : int -> t
  (** [create len] allocates a bigstring of length [len]. *)

  val of_string : ?off:int -> ?len:int -> string -> t
  (** [of_string ?off ?len str] allocates a bigstring and copies the contents
      of [str] into it. if [off] or [len] are provided, [t] will only have
      length [len] and only the specified range of the string will be copied
      into it. *)

  val length : t -> int
  (** [length t] returns the length of the bigstring. *)

  val get        : t -> int -> char
  val unsafe_get : t -> int -> char
  (** [get        t n] returns the nth byte of [t] as a [char].
      [unsafe_get t n] does the same but will not perform bounds checking. *)

  val set        : t -> int -> char -> unit
  val unsafe_set : t -> int -> char -> unit
  (** [set        t n] returns the nth byte of [t] as a [char].
      [unsafe_set t n] does the same but will not perform bounds checking. *)

  val sub : off:int -> ?len:int -> t -> t
  (** [sub ~off ?len t] returns a sub-view into the bigstring [t], specified by
      [off] and [len]. This is a {i non-copying operation}: [t] and the
      returned sub-view will share underlying bytes. Modifying one will modify
      the other. *)

  val blit : t -> int -> t -> int -> int -> unit
  val blit_from_string : string  -> int -> t -> int -> int -> unit
  val blit_from_bytes  : Bytes.t -> int -> t -> int -> int -> unit

  val to_string : ?off:int -> ?len:int -> t -> string
end

module Client_handshake : sig
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
  val next_write_operation : t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

  val read : t -> Bigstring.t -> off:int -> len:int -> int
  val report_write_result : t -> [`Ok of int | `Closed ] -> unit

  val yield_writer : t -> (unit -> unit) -> unit

  val close : t -> unit
end

module Wsd : sig
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
    -> Bigstring.t
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

  val next : t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]
  val report_result : t -> [`Ok of int | `Closed ] -> unit

  val is_closed : t -> bool

  val when_ready_to_write : t -> (unit -> unit) -> unit
end

module Client_connection : sig
  module IOVec = Httpaf.IOVec

  type t

  type error =
    [ Httpaf.Client_connection.error
    | `Handshake_failure of Httpaf.Response.t * [`read] Httpaf.Body.t ]

  type input_handlers = Client_websocket.input_handlers =
    { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
    ; eof   : unit                                                                          -> unit }

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
  val next_write_operation : t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

  val read : t -> Bigstring.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstring.t -> off:int -> len:int -> int
  val report_write_result : t -> [`Ok of int | `Closed ] -> unit

  val yield_writer : t -> (unit -> unit) -> unit

  val close : t -> unit
end

module Server_connection : sig
  module IOVec = Httpaf.IOVec

  type 'handle t

  type input_handlers =
    { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
    ; eof   : unit                                                                          -> unit }

  val create
    : sha1 : (string -> string)
    -> fd : 'handle
    -> websocket_handler : (Wsd.t -> input_handlers)
    -> 'handle t

  val upgrade
    : sha1 : (string -> string)
    -> reqd:'a Httpaf.Reqd.t
    -> ?headers: Httpaf.Headers.t
    -> (Wsd.t -> input_handlers)
    -> 'handle t

  val next_read_operation  : _ t -> [ `Read | `Yield | `Close ]
  val next_write_operation : _ t -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

  val read : _ t -> Bigstring.t -> off:int -> len:int -> int
  val read_eof : _ t -> Bigstring.t -> off:int -> len:int -> int
  val report_write_result : _ t -> [`Ok of int | `Closed ] -> unit

  val yield_reader : _ t -> (unit -> unit) -> unit
  val yield_writer : _ t -> (unit -> unit) -> unit

  val close : _ t -> unit
end

module Websocket : sig
  module Opcode : sig
    type standard_non_control =
      [ `Continuation
      | `Text
      | `Binary ]

    type standard_control =
      [ `Connection_close
      | `Ping
      | `Pong ]

    type standard =
      [ standard_non_control
      | standard_control ]

    type t =
      [ standard
      | `Other of int ]

    val code   : t -> int

    val of_code     : int -> t option
    val of_code_exn : int -> t

    val to_int : t -> int

    val of_int     : int -> t option
    val of_int_exn : int -> t

    val pp_hum : Format.formatter -> t -> unit
  end

  module Close_code : sig
    type standard =
      [ `Normal_closure
      | `Going_away
      | `Protocol_error
      | `Unsupported_data
      | `No_status_rcvd
      | `Abnormal_closure
      | `Invalid_frame_payload_data
      | `Policy_violation
      | `Message_too_big
      | `Mandatory_ext
      | `Internal_server_error
      | `TLS_handshake ]

    type t =
      [ standard | `Other of int ]

    val code : t -> int

    val of_code     : int -> t option
    val of_code_exn : int -> t

    val to_int : t -> int

    val of_int     : int -> t option
    val of_int_exn : int -> t
  end

  module Frame : sig
    type t

    val is_fin   : t -> bool
    val rsv      : t -> int

    val opcode   : t -> Opcode.t

    val has_mask : t -> bool
    val mask     : t -> unit
    val unmask   : t -> unit

    val mask_exn : t -> int32

    val length : t -> int

    val payload_length : t -> int
    val with_payload   : t -> f:(Bigstring.t -> off:int -> len:int -> 'a) -> 'a

    val copy_payload       : t -> Bigstring.t
    val copy_payload_bytes : t -> Bytes.t

    val parse : t Angstrom.t

    val serialize_control : ?mask:int32 -> Faraday.t -> opcode:Opcode.standard_control -> unit

    val schedule_serialize
      :  ?mask:int32
      -> Faraday.t
      -> is_fin:bool
      -> opcode:Opcode.t
      -> payload:Bigstring.t
      -> off:int
      -> len:int
      -> unit

    val schedule_serialize_bytes
      :  ?mask:int32
      -> Faraday.t
      -> is_fin:bool
      -> opcode:Opcode.t
      -> payload:Bytes.t
      -> off:int
      -> len:int
      -> unit

    val serialize_bytes
      :  ?mask:int32
      -> Faraday.t
      -> is_fin:bool
      -> opcode:Opcode.t
      -> payload:Bytes.t
      -> off:int
      -> len:int
      -> unit
  end
end
