open Core

let () =
  let target_area =
    In_channel.read_lines "../input" |> List.hd_exn |> Common.parse_target_area
  in

  let velocities =
    List.concat_map (Common.range (-500) 1000) ~f:(fun dy ->
        List.map (Common.range (-500) 1000) ~f:(fun dx ->
            if Common.run (dx, dy) target_area then Some (dx, dy) else None))
    |> List.filter ~f:Option.is_some
    |> List.map ~f:(fun x -> Option.value_exn x)
  in

  List.length velocities |> Printf.printf "%d\n"
