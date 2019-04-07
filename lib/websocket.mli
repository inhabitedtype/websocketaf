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
  val mask     : t -> int32 option
  val mask_exn : t -> int32

  val mask_inplace   : t -> unit
  val unmask_inplace : t -> unit

  val payload_length : t -> int
  val with_payload   : t -> f:(Bigstringaf.t -> off:int -> len:int -> 'a) -> 'a

  val copy_payload       : t -> Bigstringaf.t
  val copy_payload_bytes : t -> Bytes.t

  val parse : t Angstrom.t

  val serialize_control
    : ?mask:int32
    -> Faraday.t
    -> opcode:Opcode.standard_control
    -> unit

  val schedule_serialize 
    :  ?mask:int32
    -> Faraday.t
    -> is_fin:bool 
    -> opcode:Opcode.t 
    -> payload:Bigstringaf.t
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
