open Core
open Parse

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
