open Core

let max_y_position yv = if yv <= 0 then 0 else yv * (yv + 1) / 2

let () =
  let target_area =
    In_channel.read_lines "../input" |> List.hd_exn |> Common.parse_target_area
  in

  let max_y_velocity =
    (* iterate over increasingly larger y velocities, trying to find an x velocity which makes it work *)
    List.map (Common.range 0 500) ~f:(fun dy ->
        List.fold_until (Common.range 0 100) ~init:0
          ~f:(fun _ dx ->
            if Common.run (dx, dy) target_area then Stop (Some (dx, dy))
            else Continue 0)
          ~finish:(fun _ -> None))
    |> List.filter ~f:Option.is_some
    |> List.map ~f:(fun x -> Option.value_exn x |> snd)
    |> List.max_elt ~compare
    |> fun x -> Option.value_exn x
  in

  max_y_position max_y_velocity |> Printf.printf "%d\n"
