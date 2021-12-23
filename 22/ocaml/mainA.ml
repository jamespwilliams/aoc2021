open Core
open Common

let in_bounds x = x <= 50 && x >= -50

let () =
  let input =
    In_channel.read_lines "../input"
    |> parse_input
    |> List.filter ~f:(fun (_, ((x1, x2), (y1, y2), (z1, z2))) ->
           List.for_all ~f:in_bounds [ x1; x2; y1; y2; z1; z2 ])
  in

  let cube =
    Array.init 101 ~f:(fun _ ->
        Array.init 101 ~f:(fun _ -> Array.create ~len:101 false))
  in

  let apply_instruction cube (kind, ((x1, x2), (y1, y2), (z1, z2))) =
    for x = x1 to x2 - 1 do
      for y = y1 to y2 - 1 do
        for z = z1 to z2 - 1 do
          if in_bounds x && in_bounds y && in_bounds z then
            cube.(x + 50).(y + 50).(z + 50) <-
              (match kind with On -> true | Off -> false)
        done
      done
    done
  in

  List.iter input ~f:(apply_instruction cube);

  let sum a ~f = Array.sum (module Int) a ~f in

  sum cube ~f:(sum ~f:(sum ~f:(fun x -> if x then 1 else 0)))
  |> Printf.printf "%d\n"
