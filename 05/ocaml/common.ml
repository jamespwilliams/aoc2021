open Core

let parse_vent_line line =
  let parse_vent_position pos =
    match String.split ~on:',' pos with
    | [ a; b ] -> (int_of_string a, int_of_string b)
    | _ -> failwith "invalid vent position"
  in
  match String.split ~on:' ' line with
  | [ a; _; b ] -> (parse_vent_position a, parse_vent_position b)
  | _ -> failwith "invalid vent line"

let points_between ((x1, y1), (x2, y2)) =
  let range ~start ~len = List.init (len + 1) ~f:(fun x -> x + start) in

  let dx = abs (x1 - x2) and dy = abs (y1 - y2) in
  let startx, starty = (min x1 x2, min y1 y2) in

  let xs = range ~start:startx ~len:dx in
  let ys = range ~start:starty ~len:dy in

  if dx > 0 && dy > 0 then
    if (x1 - x2 >= 0 && y1 - y2 >= 0) || (x1 - x2 < 0 && y1 - y2 < 0) then
      List.map2_exn xs ys ~f:(fun x y -> (x, y))
    else List.map2_exn (List.rev xs) ys ~f:(fun x y -> (x, y))
  else if dx > 0 then List.map xs ~f:(fun x -> (x, starty))
  else List.map ys ~f:(fun y -> (startx, y))

let count_line_crossings vents =
  let ocean = Array.init 1000 ~f:(fun _ -> Array.create ~len:1000 0) in

  List.iter vents ~f:(fun vent ->
      let points = points_between vent in
      List.iter points ~f:(fun (x, y) -> ocean.(x).(y) <- ocean.(x).(y) + 1));

  Array.map ocean ~f:(fun row -> Array.count row ~f:(fun v -> v > 1))
  |> Array.fold ~f:( + ) ~init:0
