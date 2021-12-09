open Core

let input = In_channel.read_lines "../input" |> Seafloor.from_lines

let () =
  Seafloor.sumi input ~f:(fun point value ->
      if List.for_all (Seafloor.neighbours input point) ~f:(fun x -> x > value)
      then value + 1
      else 0)
  |> string_of_int |> print_endline
