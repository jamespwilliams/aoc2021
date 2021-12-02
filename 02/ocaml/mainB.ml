open Core

let end_position =
  Common.read_commands "../input"
  |> List.fold ~init:(0, 0, 0) ~f:(fun pos com ->
         match (pos, com) with
         | (x, depth, aim), (Common.Up, n) -> (x, depth, aim - n)
         | (x, depth, aim), (Common.Down, n) -> (x, depth, aim + n)
         | (x, depth, aim), (Common.Forward, n) ->
             (x + n, depth + (aim * n), aim))

let () =
  let x, depth, _ = end_position in
  Printf.printf "%d" (x * depth)
