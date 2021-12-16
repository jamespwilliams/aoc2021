open Core

type packet =
  | Literal of { version : int; value : int }
  | Operator of { version : int; type_id : int; subpackets : packet list }

let rec bit_string_of_hex_string = function
  | [] -> []
  | c :: l -> (
      let rest = bit_string_of_hex_string l in
      match c with
      | '0' -> 0 :: 0 :: 0 :: 0 :: rest
      | '1' -> 0 :: 0 :: 0 :: 1 :: rest
      | '2' -> 0 :: 0 :: 1 :: 0 :: rest
      | '3' -> 0 :: 0 :: 1 :: 1 :: rest
      | '4' -> 0 :: 1 :: 0 :: 0 :: rest
      | '5' -> 0 :: 1 :: 0 :: 1 :: rest
      | '6' -> 0 :: 1 :: 1 :: 0 :: rest
      | '7' -> 0 :: 1 :: 1 :: 1 :: rest
      | '8' -> 1 :: 0 :: 0 :: 0 :: rest
      | '9' -> 1 :: 0 :: 0 :: 1 :: rest
      | 'A' -> 1 :: 0 :: 1 :: 0 :: rest
      | 'B' -> 1 :: 0 :: 1 :: 1 :: rest
      | 'C' -> 1 :: 1 :: 0 :: 0 :: rest
      | 'D' -> 1 :: 1 :: 0 :: 1 :: rest
      | 'E' -> 1 :: 1 :: 1 :: 0 :: rest
      | 'F' -> 1 :: 1 :: 1 :: 1 :: rest
      | _ -> failwith "invalid hex digit")

let int_of_bit_string l =
  List.rev l
  |> List.fold ~init:(1, 0) ~f:(fun (power, acc) d ->
         (power * 2, acc + (power * d)))
  |> snd

let packet_version : packet -> int =
 fun packet ->
  match packet with Literal l -> l.version | Operator o -> o.version

let consume_literal_value : int list -> int * int list =
 fun l ->
  let rec aux : int list -> int list * int list = function
    | 0 :: a :: b :: c :: d :: l -> ([ a; b; c; d ], l)
    | 1 :: a :: b :: c :: d :: l ->
        let data, rest = aux l in
        (a :: b :: c :: d :: data, rest)
    | _ -> failwith "invalid literal"
  in
  let data, rest = aux l in
  (int_of_bit_string data, rest)

let rec consume_packet : int list -> packet * int list =
 fun data ->
  let consume_int l ~length =
    let i, rest = List.split_n l length in
    (int_of_bit_string i, rest)
  in

  let version, data = consume_int data ~length:3 in
  let type_id, data = consume_int data ~length:3 in

  match type_id with
  | 4 ->
      let value, data = consume_literal_value data in
      (Literal { version; value }, data)
  | _ ->
      let length_type, data = consume_int data ~length:1 in

      let subpackets, data =
        if length_type = 0 then
          let subpacket_length, data = consume_int data ~length:15 in
          consume_subpackets_by_bitlength subpacket_length data
        else
          let subpacket_count, data = consume_int data ~length:11 in
          consume_subpackets_by_count subpacket_count data
      in
      (Operator { version; type_id; subpackets }, data)

and consume_subpackets_by_bitlength : int -> int list -> packet list * int list
    =
 fun bitlength data ->
  let original_length = List.length data in
  let data = ref data in
  let packets : packet list ref = ref [] in

  while original_length - List.length !data < bitlength do
    let p, new_data = consume_packet !data in
    packets := p :: !packets;
    data := new_data
  done;

  (List.rev !packets, !data)

and consume_subpackets_by_count : int -> int list -> packet list * int list =
 fun count data ->
  let i = ref 0 in
  let data = ref data in
  let packets : packet list ref = ref [] in

  while !i < count do
    let p, new_data = consume_packet !data in
    packets := p :: !packets;
    data := new_data;
    i := !i + 1
  done;

  (List.rev !packets, !data)

let rec version_sum = function
  | Literal l -> l.version
  | Operator o ->
      o.version
      + List.sum
          (module Int)
          o.subpackets
          ~f:(fun subpacket -> version_sum subpacket)

let () =
  In_channel.read_lines "../input"
  |> List.hd_exn |> String.to_list |> bit_string_of_hex_string |> consume_packet
  |> fst |> version_sum |> Printf.printf "%d\n"
