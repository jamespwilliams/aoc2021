open Core

let () =
  In_channel.read_lines "../input"
  |> List.map ~f:Common.parse_vent_line
  |> Common.count_line_crossings |> string_of_int |> print_endline
