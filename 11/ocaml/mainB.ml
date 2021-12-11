open Core

let input = In_channel.read_lines "../input" |> Octopi.from_lines

let range n = List.init n ~f:(fun x -> x)

let find_all_flash octopi =
  let rec aux octopi target count =
    let flashes = Octopi.step octopi in
    if flashes = target then count else aux octopi target (count + 1)
  in
  let yb, xb = Octopi.bounds octopi in
  aux octopi ((yb + 1) * (xb + 1)) 1

let () = find_all_flash input |> string_of_int |> print_endline
