module IOVec = Httpaf.IOVec

type t =
  { reader : [`Parse of string list * string] Reader.t
  ; wsd    : Wsd.t }

type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

let create ~websocket_handler =
  let mode         = `Server in
  let wsd          = Wsd.create mode in
  let { frame; _ } = websocket_handler wsd in
  { reader = Reader.create frame
  ; wsd
  }

let next_read_operation t =
  Reader.next t.reader

let next_write_operation t =
  Wsd.next t.wsd

let read t bs ~off ~len =
  Reader.read_with_more t.reader bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  Reader.read_with_more t.reader bs ~off ~len Complete

let report_write_result t result =
  Wsd.report_result t.wsd result

let yield_writer t k =
  if Wsd.is_closed t.wsd
  then begin
    Wsd.close t.wsd;
    k ()
  end else
    Wsd.when_ready_to_write t.wsd k

let close { wsd; _ } =
  Wsd.close wsd
