open Core

let read_crabs filename =
  In_channel.read_lines filename
  |> List.hd_exn |> String.split ~on:',' |> List.map ~f:int_of_string

let range n = List.init n ~f:(fun x -> x)

let find_minimum_fuel crabs calc_fuel_cost =
  let maximum_displacement = Option.value_exn (List.max_elt crabs ~compare) in

  List.min_elt
    (List.map (range maximum_displacement) ~f:(calc_fuel_cost crabs))
    ~compare
  |> fun x -> Option.value_exn x
