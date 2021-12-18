open Core

let () =
  let input_numbers =
    In_channel.read_lines "../input" |> List.map ~f:Common.parse_number
  in

  List.fold
    (List.tl_exn input_numbers)
    ~init:(List.hd_exn input_numbers)
    ~f:Common.add
  |> Common.magnitude |> Printf.printf "%d\n"
