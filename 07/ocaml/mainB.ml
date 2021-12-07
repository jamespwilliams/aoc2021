open Core

let crabs =
  In_channel.read_lines "../input"
  |> List.hd_exn |> String.split ~on:',' |> List.map ~f:int_of_string

let triangle n = n * (n + 1) / 2

let fuel_cost crabs target =
  List.fold crabs ~init:0 ~f:(fun acc crab ->
      acc + triangle (abs (crab - target)))

let range n = List.init n ~f:(fun x -> x)

let () =
  List.min_elt (List.map (range 2000) ~f:(fuel_cost crabs)) ~compare |> fun x ->
  Option.value_exn x |> string_of_int |> print_endline
