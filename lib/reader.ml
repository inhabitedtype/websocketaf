module AU = Angstrom.Unbuffered

type 'error parse_state =
  | Done
  | Fail    of 'error
  | Partial of (Bigstring.t -> off:int -> len:int -> AU.more -> (unit, 'error) result AU.state)

type 'error t =
  { parser : unit Angstrom.t
  ; mutable parse_state : 'error parse_state
  ; mutable closed      : bool }

let create frame_handler  =
  let open Angstrom in
  Websocket.parser
  >>| fun bs ->
    let is_fin = Frame.is_fin bs in
    let opcode = Frame.opcode bs in
    Frame.unmask bs;
    frame_handler ~is_fin ~opcode bs ~off ~len:(Bigstring.length bs)
;;

let transition t state =
  match state with
  | AU.Done(consumed, Ok ())
  | AU.Fail(0 as consumed, _, _) ->
    t.parse_state <- Done;
    consumed
  | AU.Done(consumed, Error error) ->
    t.parse_state <- Fail error;
    consumed
  | AU.Fail(consumed, marks, msg) ->
    t.parse_state <- Fail (`Parse(marks, msg));
    consumed
  | AU.Partial { committed; continue } ->
    t.parse_state <- Partial continue;
    committed
and start t state =
    match state with
    | AU.Done _         -> failwith "httpaf.Parse.unable to start parser"
    | AU.Fail(0, marks, msg) ->
      t.parse_state <- Fail (`Parse(marks, msg))
    | AU.Partial { committed = 0; continue } ->
      t.parse_state <- Partial continue
    | _ -> assert false
;;
