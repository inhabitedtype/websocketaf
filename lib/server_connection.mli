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
