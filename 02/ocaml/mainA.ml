open Core

let end_position =
  Common.read_commands "../input"
  |> List.fold ~init:(0, 0) ~f:(fun pos command ->
         match (pos, command) with
         | (x, depth), (Common.Up, n) -> (x, depth - n)
         | (x, depth), (Common.Down, n) -> (x, depth + n)
         | (x, depth), (Common.Forward, n) -> (x + n, depth))

let () =
  let x, depth = end_position in
  Printf.printf "%d" (x * depth)
