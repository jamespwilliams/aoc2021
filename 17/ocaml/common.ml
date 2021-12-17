open Core

type point = int * int

type velocity = int * int

type target_area = (int * int) * (int * int)

type step_result = NotThereYet | Hit | Overshot [@@deriving show]

type run_result = Hit | Miss [@@deriving show]

let input_regex = Re2.create_exn ".*x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)"

let parse_target_area line =
  Re2.find_submatches_exn input_regex line
  |> Array.to_list |> List.tl_exn
  |> List.map ~f:(fun o -> Option.value_exn o |> int_of_string)
  |> fun l ->
  match l with
  | [ a; b; c; d ] -> ((a, b), (c, d))
  | _ -> failwith "invalid input"

let step_velocity : velocity -> velocity =
 fun (dx, dy) ->
  ((if dx = 0 then dx else if dx > 0 then dx - 1 else dx + 1), dy - 1)

let calculate_step_result : target_area -> point -> step_result =
 fun ((tx0, tx1), (ty0, ty1)) (x, y) ->
  if x >= tx0 && x <= tx1 && y >= ty0 && y <= ty1 then Hit
  else if x > tx1 || y < ty0 then Overshot
  else NotThereYet

let step : point -> velocity -> target_area -> point * velocity * step_result =
 fun (x, y) (dx, dy) target ->
  let x, y = (x + dx, y + dy) in
  let dx, dy = step_velocity (dx, dy) in
  ((x, y), (dx, dy), calculate_step_result target (x, y))

let range a b = List.init (b - a) ~f:(fun i -> i + a)

let run velocity target =
  List.fold_until
    ~init:((0, 0), velocity)
    (range 0 10000)
    ~f:(fun (p, v) _ ->
      let p, v, result = step p v target in
      match result with
      | NotThereYet -> Continue (p, v)
      | Hit -> Stop true
      | Overshot -> Stop false)
    ~finish:(fun _ -> failwith "step limit reached")
