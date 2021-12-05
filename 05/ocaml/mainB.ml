open Core
open Common

let () =
  let vents = In_channel.read_lines "../input" |> List.map ~f:parse_vent_line in

  count_line_crossings vents |> string_of_int |> print_endline
