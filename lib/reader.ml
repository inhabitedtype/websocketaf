module AU = Angstrom.Unbuffered

type 'error parse_state =
  | Done
  | Fail    of 'error
  | Partial of (Bigstring.t -> off:int -> len:int -> AU.more -> unit AU.state)

type 'error t =
  { parser : unit Angstrom.t
  ; mutable parse_state : 'error parse_state
  ; mutable closed      : bool }

let create frame_handler =
  let parser =
    let open Angstrom in
    Websocket.Frame.parse
    >>| fun frame ->
      let is_fin = Websocket.Frame.is_fin frame in
      let opcode = Websocket.Frame.opcode frame in
      Websocket.Frame.unmask frame;
      Websocket.Frame.with_payload frame ~f:(frame_handler ~opcode ~is_fin)
  in
  { parser
  ; parse_state = Done
  ; closed      = false
  }
;;

let transition t state =
  match state with
  | AU.Done(consumed, ())
  | AU.Fail(0 as consumed, _, _) ->
    t.parse_state <- Done;
    consumed
  | AU.Fail(consumed, marks, msg) ->
    t.parse_state <- Fail (`Parse(marks, msg));
    consumed
  | AU.Partial { committed; continue } ->
    t.parse_state <- Partial continue;
    committed
and start t state =
    match state with
    | AU.Done _         -> failwith "websocketaf.Reader.unable to start parser"
    | AU.Fail(0, marks, msg) ->
      t.parse_state <- Fail (`Parse(marks, msg))
    | AU.Partial { committed = 0; continue } ->
      t.parse_state <- Partial continue
    | _ -> assert false
;;

let next t =
  match t.parse_state with
  | Done ->
    if t.closed
    then `Close
    else `Read
  | Fail failure -> `Error failure
  | Partial _ -> `Read
;;

let rec read_with_more t bs ~off ~len more =
  let consumed =
    match t.parse_state with
    | Fail _ -> 0
    | Done   ->
      start t (AU.parse t.parser);
      read_with_more  t bs ~off ~len more;
    | Partial continue ->
      transition t (continue bs more ~off ~len)
  in
  begin match more with
  | Complete -> t.closed <- true;
  | Incomplete -> ()
  end;
  consumed
;;
