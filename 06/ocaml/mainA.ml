open Core

let input =
  In_channel.read_lines "../input" |> fun lines ->
  List.nth_exn lines 0 |> String.split ~on:',' |> List.map ~f:int_of_string

let step_day fishes =
  let rec step_day_aux acc = function
    | fish :: fishes -> (
        match fish with
        | 0 -> step_day_aux (6 :: 8 :: acc) fishes
        | n -> step_day_aux ((n - 1) :: acc) fishes)
    | _ -> acc
  in
  step_day_aux [] fishes

let range n = List.init n ~f:(fun x -> x)

let () =
  List.fold (range 80) ~init:input ~f:(fun fishes _ -> step_day fishes)
  |> List.length |> string_of_int |> print_endline
