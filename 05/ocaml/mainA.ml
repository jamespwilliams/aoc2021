open Core

let () =
  In_channel.read_lines "../input"
  |> List.map ~f:Common.parse_vent_line
  |> List.filter ~f:(fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
  |> Common.count_line_crossings |> string_of_int |> print_endline
