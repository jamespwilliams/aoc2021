open Core

type t = int Array.t Array.t

let from_lines lines =
  lines
  |> List.map ~f:(fun line ->
         String.to_list line |> List.map ~f:Char.get_digit_exn |> Array.of_list)
  |> Array.of_list

let print octopi =
  Printf.printf "%s"
    (Array.fold ~init:"" octopi ~f:(fun acc row ->
         Array.fold ~init:acc row ~f:(fun acc value ->
             acc ^ string_of_int value)
         ^ "\n"))

let iteri octopi ~f =
  Array.iteri octopi ~f:(fun iy row ->
      Array.iteri row ~f:(fun ix value -> f (iy, ix) value))

let bounds octopi =
  let ybound = Array.length octopi - 1
  and xbound = Array.length octopi.(0) - 1 in
  (ybound, xbound)

let get octopi (y, x) =
  let ybound, xbound = bounds octopi in
  if y >= 0 && y <= ybound && x >= 0 && x <= xbound then Some octopi.(y).(x)
  else None

let get_exn octopi pos = Option.value_exn (get octopi pos)

let neighboursi octopi (y, x) =
  [
    (y - 1, x - 1);
    (y - 1, x);
    (y - 1, x + 1);
    (y, x - 1);
    (y, x + 1);
    (y + 1, x - 1);
    (y + 1, x);
    (y + 1, x + 1);
  ]
  |> List.map ~f:(fun p -> (p, get octopi p))
  |> List.filter ~f:(fun (_, o) -> Option.is_some o)
  |> List.map ~f:(fun (p, o) -> (p, Option.value_exn o))

let sumi octopi ~f =
  Array.foldi octopi ~init:0 ~f:(fun iy acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun ix acc value -> acc + f (iy, ix) value))

let increment octopi (y, x) = octopi.(y).(x) <- octopi.(y).(x) + 1

let reset octopi (y, x) = octopi.(y).(x) <- 0

(* flash sets the the octopus' position to 0, increments all its neighbours, and flashes all octopi who were flashed
   over the 9 threshold. octopi with a value of 0 are always ignored, as they've flashed this turn. *)
let rec flash octopi p =
  let v = Option.value_exn (get octopi p) in
  if v <= 9 then 0
  else (
    reset octopi p;
    List.iter (neighboursi octopi p) ~f:(fun (p, v) ->
        if v > 0 then increment octopi p else ());
    1
    + List.sum
        (module Int)
        (neighboursi octopi p)
        ~f:(fun (p, _) -> flash octopi p))

let step octopi =
  iteri octopi ~f:(fun position _ -> increment octopi position);
  let flashes = sumi octopi ~f:(fun p _ -> flash octopi p) in
  flashes
