open Core

let input = In_channel.read_lines "../input" |> Octopi.from_lines

let range n = List.init n ~f:(fun x -> x)

let () =
  List.sum (module Int) (range 100) ~f:(fun _ -> Octopi.step input)
  |> string_of_int |> print_endline
