open Core
open Common

let () =
  let vents = In_channel.read_lines "../input" |> List.map ~f:parse_vent_line in

  let straight_vents =
    List.filter vents ~f:(fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
  in

  count_line_crossings straight_vents |> string_of_int |> print_endline
