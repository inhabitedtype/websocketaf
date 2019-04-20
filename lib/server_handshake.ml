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

type 'handle t =
  { connection : 'handle Httpaf.Server_connection.t
  }

let create
    ~request_handler
  =
  let connection =
    Httpaf.Server_connection.create
      request_handler
  in
  { connection }
;;

let next_read_operation t =
  Httpaf.Server_connection.next_read_operation t.connection

let next_write_operation t =
  Httpaf.Server_connection.next_write_operation t.connection

let read t =
  Httpaf.Server_connection.read t.connection

let read_eof t =
  Httpaf.Server_connection.read_eof t.connection

let report_write_result t =
  Httpaf.Server_connection.report_write_result t.connection

let yield_reader t =
  Httpaf.Server_connection.yield_reader t.connection

let yield_writer t =
  Httpaf.Server_connection.yield_writer t.connection

let close t =
  Httpaf.Server_connection.shutdown t.connection
