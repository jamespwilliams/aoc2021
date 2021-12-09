open Core

type t = int Array.t Array.t

let iteri seafloor ~f =
  Array.iteri seafloor ~f:(fun iy row ->
      Array.iteri row ~f:(fun ix value -> f (iy, ix) value))

let bounds seafloor =
  let ybound = Array.length seafloor - 1
  and xbound = Array.length seafloor.(0) - 1 in
  (ybound, xbound)

let get seafloor (y, x) =
  let ybound, xbound = bounds seafloor in
  if y >= 0 && y <= ybound && x >= 0 && x <= xbound then Some seafloor.(y).(x)
  else None

let get_exn seafloor pos = Option.value_exn (get seafloor pos)

let neighboursi seafloor (y, x) =
  List.map
    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
    ~f:(fun p -> (p, get seafloor p))
  |> List.filter ~f:(fun (_, o) -> Option.is_some o)
  |> List.map ~f:(fun (p, o) -> (p, Option.value_exn o))

let neighbours seafloor pos = neighboursi seafloor pos |> List.map ~f:snd

let sumi seafloor ~f =
  Array.foldi seafloor ~init:0 ~f:(fun iy acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun ix acc value -> acc + f (iy, ix) value))

let from_lines lines =
  lines
  |> List.map ~f:(fun line ->
         String.to_list line |> List.map ~f:Char.get_digit_exn |> Array.of_list)
  |> Array.of_list
