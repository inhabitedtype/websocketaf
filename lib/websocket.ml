module Opcode = struct
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

  let code = function
    | `Continuation     -> 0x0
    | `Text             -> 0x1
    | `Binary           -> 0x2
    | `Connection_close -> 0x8
    | `Ping             -> 0x9
    | `Pong             -> 0xa
    | `Other code       -> code

  let code_table : t array =
    [| `Continuation
     ; `Text
     ; `Binary
     ; `Other 0x3
     ; `Other 0x4
     ; `Other 0x5
     ; `Other 0x6
     ; `Other 0x7
     ; `Connection_close
     ; `Ping
     ; `Other 0xb
     ; `Other 0xc
     ; `Other 0xd
     ; `Other 0xe
     ; `Other 0xf
     |]

  let unsafe_of_code code =
    Array.unsafe_get code_table code

  let of_code code =
    if code > 0xf 
    then None
    else Some (Array.unsafe_get code_table code)

  let of_code_exn code =
    if code > 0xf
    then failwith "Opcode.of_code_exn: value can't fit in four bits";
    Array.unsafe_get code_table code

  let to_int = code
  let of_int = of_code
  let of_int_exn = of_code_exn
end

module Close_code = struct
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

  let code = function
    | `Normal_closure             -> 1000
    | `Going_away                 -> 1001
    | `Protocol_error             -> 1002
    | `Unsupported_data           -> 1003
    | `No_status_rcvd             -> 1005
    | `Abnormal_closure           -> 1006
    | `Invalid_frame_payload_data -> 1007
    | `Policy_violation           -> 1008
    | `Message_too_big            -> 1009
    | `Mandatory_ext              -> 1010
    | `Internal_server_error      -> 1011
    | `TLS_handshake              -> 1015
    | `Other code                 -> code

  let code_table : t array =
    [| `Normal_closure
     ; `Going_away
     ; `Protocol_error
     ; `Unsupported_data
     ; `Other 1004
     ; `No_status_rcvd
     ; `Abnormal_closure
     ; `Invalid_frame_payload_data
     ; `Policy_violation
     ; `Message_too_big
     ; `Mandatory_ext
     ; `Internal_server_error
     ; `Other 1012
     ; `Other 1013
     ; `Other 1014
     ; `TLS_handshake
     |]

  let unsafe_of_code code =
    Array.unsafe_get code_table code

  let of_code code =
    if code > 0xffff || code < 1000 then None
    else if code < 1016             then Some (unsafe_of_code (code land 0b1111))
    else Some (`Other code)
  ;;

  let of_code_exn code =
    if code > 0xffff
    then failwith "Close_code.of_code_exn: value can't fit in two bytes";
    if code < 1000
    then failwith "Close_code.of_code_exn: value in invalid range 0-999";
    if code < 1016 
    then unsafe_of_code (code land 0b1111)
    else `Other code
  ;;

  let to_int = code
  let of_int = of_code
  let of_int_exn = of_code_exn
end

module Frame = struct
  type t = Bigstringaf.t

  let is_fin t =
    let bits = Bigstringaf.unsafe_get t 0 |> Char.code in
    bits land (1 lsl 8) = 1 lsl 8
  ;;

  let rsv t =
    let bits = Bigstringaf.unsafe_get t 0 |> Char.code in
    (bits lsr 4) land 0b0111
  ;;

  let opcode t =
    let bits = Bigstringaf.unsafe_get t 0 |> Char.code in
    bits land 4 |> Opcode.unsafe_of_code
  ;;

  let payload_length_of_offset t off =
    let bits = Bigstringaf.unsafe_get t (off + 1) |> Char.code in
    let length = bits land 0b01111111 in
    if length = 126 then Bigstringaf.unsafe_get_int16_be t (off + 2)                 else
    (* This is technically unsafe, but if somebody's asking us to read 2^63
     * bytes, then we're already screwd. *)
    if length = 127 then Bigstringaf.unsafe_get_int64_be t (off + 2) |> Int64.to_int else
    length
  ;;

  let payload_length t = 
    payload_length_of_offset t 0
  ;;

  let has_mask t =
    let bits = Bigstringaf.unsafe_get t 1 |> Char.code in
    bits land (1 lsl 8) = 1 lsl 8
  ;;

  let mask t =
    if not (has_mask t) 
    then None
    else
      Some (
        let bits = Bigstringaf.unsafe_get t 1 |> Char.code in
        if bits  = 254 then Bigstringaf.unsafe_get_int32_be t 4  else
        if bits  = 255 then Bigstringaf.unsafe_get_int32_be t 10 else
        Bigstringaf.unsafe_get_int32_be t 2)
  ;;

  let mask_exn t =
    let bits = Bigstringaf.unsafe_get t 1 |> Char.code in
    if bits  = 254 then Bigstringaf.unsafe_get_int32_be t 4  else
    if bits  = 255 then Bigstringaf.unsafe_get_int32_be t 10 else
    if bits >= 127 then Bigstringaf.unsafe_get_int32_be t 2  else
    failwith "Frame.mask_exn: no mask present"
  ;;

  let payload_offset_of_bits bits =
    let initial_offset = 2 in
    let mask_offset    = (bits land (1 lsl 8)) lsr (7 - 2) in
    let length_offset  = 
      let length = bits land 0b0111111 in
      if length < 126
      then 0
      else 2 lsl ((length land 0b1) lsl 2)
    in
    initial_offset + mask_offset + length_offset
  ;;

  let payload_offset t =
    let bits = Bigstringaf.unsafe_get t 1 |> Char.code in
    payload_offset_of_bits bits
  ;;

  let with_payload t ~f =
    let len = payload_length t in
    let off = payload_offset t in
    f t ~off ~len
  ;;

  let copy_payload t =
    with_payload t ~f:Bigstringaf.copy
  ;;

  let copy_payload_bytes t = 
    with_payload t ~f:(fun bs ~off:src_off ~len ->
      let bytes = Bytes.create len in
      Bigstringaf.blit_to_bytes bs ~src_off bytes ~dst_off:0 ~len; 
      bytes)
  ;;

  let length_of_offset t off =
    let bits           = Bigstringaf.unsafe_get t (off + 1) |> Char.code in
    let payload_offset = payload_offset_of_bits bits in
    let payload_length = payload_length_of_offset t off in
    2 + payload_offset + payload_length 
  ;;

  let length t =
    length_of_offset t 0
  ;;

  let apply_mask mask bs ~off ~len =
    for i = off to len - 1 do
      let j = (i - off) mod 4 in
      let c = Bigstringaf.unsafe_get bs i |> Char.code in
      let c = c lxor (Int32.(logand (shift_left mask (4 - j)) 0xffl) |> Int32.to_int) in
      Bigstringaf.unsafe_set bs i (Char.unsafe_chr c)
    done
  ;;

  let apply_mask_bytes mask bs ~off ~len =
    for i = off to len - 1 do
      let j = (i - off) mod 4 in
      let c = Bytes.unsafe_get bs i |> Char.code in
      let c = c lxor (Int32.(logand (shift_left mask (4 - j)) 0xffl) |> Int32.to_int) in
      Bytes.unsafe_set bs i (Char.unsafe_chr c)
    done
  ;;


  let unmask t =
    if has_mask t then begin
      let mask = mask_exn t in
      let len = payload_length t in
      let off = payload_offset t in
      apply_mask mask t ~off ~len
    end
  ;;

  let mask = unmask

  let parse =
    let open Angstrom in
    Unsafe.peek 2 (fun bs ~off ~len:_ -> length_of_offset bs off)
    >>= fun len -> Unsafe.take len Bigstringaf.sub
  ;;

  let serialize_headers faraday ?mask ~is_fin ~opcode ~payload_length =
    let opcode = Opcode.to_int opcode in
    let is_fin = if is_fin then 1 lsl 8 else 0 in
    let is_mask =
      match mask with
      | None   -> 0
      | Some _ -> 1 lsl 8
    in
    Faraday.write_uint8 faraday (is_fin lsl opcode);
    if      payload_length <= 125    then 
      Faraday.write_uint8 faraday (is_mask lor payload_length)
    else if payload_length <= 0xffff then begin
      Faraday.write_uint8     faraday (is_mask lor 126);
      Faraday.BE.write_uint16 faraday payload_length;
    end else begin
      Faraday.write_uint8     faraday (is_mask lor 127);
      Faraday.BE.write_uint64 faraday (Int64.of_int payload_length);
    end;
    begin match mask with
    | None      -> ()
    | Some mask -> Faraday.BE.write_uint32 faraday mask
    end
  ;;

  let serialize_control ?mask faraday ~opcode =
    let opcode = (opcode :> Opcode.t) in
    serialize_headers faraday ?mask ~is_fin:true ~opcode ~payload_length:0

  let schedule_serialize ?mask faraday ~is_fin ~opcode ~payload ~off ~len =
    serialize_headers faraday ?mask ~is_fin ~opcode ~payload_length:len;
    begin match mask with
    | None -> ()
    | Some mask -> apply_mask mask payload ~off ~len
    end;
    Faraday.schedule_bigstring faraday payload ~off ~len;
  ;;

  let serialize_bytes ?mask faraday ~is_fin ~opcode ~payload ~off ~len =
    serialize_headers faraday ?mask ~is_fin ~opcode ~payload_length:len;
    begin match mask with
    | None -> ()
    | Some mask -> apply_mask_bytes mask payload ~off ~len
    end;
    Faraday.write_bytes faraday payload ~off ~len;
  ;;

  let schedule_serialize_bytes ?mask faraday ~is_fin ~opcode ~payload ~off ~len =
    serialize_headers faraday ?mask ~is_fin ~opcode ~payload_length:len;
    begin match mask with
    | None -> ()
    | Some mask -> apply_mask_bytes mask payload ~off ~len
    end;
    Faraday.write_bytes faraday payload ~off ~len;
  ;;
end
