open Core

let () =
  let input_numbers =
    In_channel.read_lines "../input" |> List.map ~f:Common.parse_number
  in

  List.concat_map input_numbers ~f:(fun n1 ->
      List.concat_map input_numbers ~f:(fun n2 ->
          [
            Common.add n1 n2 |> Common.magnitude;
            Common.add n2 n1 |> Common.magnitude;
          ]))
  |> List.max_elt ~compare
  |> fun x -> Option.value_exn x |> Printf.printf "%d\n"
