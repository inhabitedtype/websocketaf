type t =
  { reader : Reader.t
  ; wsd    : Wsd.t }

type input_handlers =
  { frame : Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                   -> unit }

let create ~websocket_handler =
  let wsd            = Wsd.create () in
  let { frame; eof } = websocket_handler wsd in
  { reader = Reader.create frame
  ; wds 
  }
