open Core

let input =
  In_channel.read_lines "../input" |> fun lines ->
  List.nth_exn lines 0 |> String.split ~on:',' |> List.map ~f:int_of_string

let range n = List.init n ~f:(fun x -> x)

let rotate_left array =
  let head = array.(0) in
  Array.iteri array ~f:(fun index _ ->
      if index > 0 then array.(index - 1) <- array.(index));
  array.(Array.length array - 1) <- head

let step_day timers =
  rotate_left timers;
  timers.(6) <- timers.(6) + timers.(8)

let () =
  let timers =
    Array.init 9 ~f:(fun index ->
        List.count input ~f:(fun fish -> fish = index))
  in

  List.iter (range 256) ~f:(fun _ -> step_day timers);
  Array.fold ~init:0 ~f:( + ) timers |> string_of_int |> print_endline
