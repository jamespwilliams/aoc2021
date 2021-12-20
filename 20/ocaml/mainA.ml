open Core

let () =
  let algorithm, image =
    In_channel.read_lines "../input" |> Common.parse_input
  in
  List.fold (Common.range 2) ~init:image ~f:(Common.step algorithm)
  |> Image.count ~f:Fn.id |> Printf.printf "%d\n"
