open! Core
open Async

module Client : sig
  val connect
    : ([`Active], [< Socket.Address.t]) Socket.t
    -> nonce             : string
    -> host              : string
    -> port              : int
    -> resource          : string
    -> error_handler : (Websocketaf.Client_connection.error -> unit)
    -> websocket_handler : (Websocketaf.Wsd.t -> Websocketaf.Client_connection.input_handlers)
    -> unit Deferred.t
end

module Server : sig
  val create_connection_handler
    :  ?config : Httpaf.Server_connection.Config.t
    -> websocket_handler : ( 'a
                           -> Websocketaf.Wsd.t
                           -> Websocketaf.Server_connection.input_handlers)
    -> error_handler     : ('a -> Httpaf.Server_connection.error_handler)
    -> ([< Socket.Address.t] as 'a)
    -> ([`Active], 'a) Socket.t
    -> unit Deferred.t


  val upgrade_connection
    :  ?config : Httpaf.Server_connection.Config.t
    -> ?headers: Httpaf.Headers.t
    -> reqd : ([`Active], [< Socket.Address.t]) Socket.t Httpaf.Reqd.t
    -> error_handler : Websocketaf.Server_connection.error_handler
    -> (Websocketaf.Wsd.t -> Websocketaf.Server_connection.input_handlers)
    -> (unit, string) Deferred.Result.t
end
