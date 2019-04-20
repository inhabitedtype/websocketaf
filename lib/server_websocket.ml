(*----------------------------------------------------------------------------
    Copyright (c) 2019 Andreas Garnaes

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module IOVec = Httpaf.IOVec

type t =
  { reader : [`Parse of string list * string] Reader.t
  ; wsd    : Wsd.t }

type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
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
