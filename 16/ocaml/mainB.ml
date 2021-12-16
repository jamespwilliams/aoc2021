open Core
open Parse

let rec evaluate = function
  | Literal l -> l.value
  | Operator o ->
      let subvalues = List.map ~f:evaluate o.subpackets in
      apply_operator subvalues o.operator

let () =
  In_channel.read_lines "../input"
  |> List.hd_exn |> String.to_list |> bit_string_of_hex_string |> consume_packet
  |> fst |> evaluate |> Printf.printf "%d\n"
