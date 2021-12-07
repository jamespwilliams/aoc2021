open Core

let triangle n = n * (n + 1) / 2

let fuel_cost crabs target =
  List.fold crabs ~init:0 ~f:(fun acc crab ->
      acc + triangle (abs (crab - target)))

let () =
  let crabs = Common.read_crabs "../input" in

  Common.find_minimum_fuel crabs fuel_cost |> string_of_int |> print_endline
