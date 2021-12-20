open Core

type t = bool Array.t Array.t

let from_lines (lines : string list) : t =
  lines
  |> List.map ~f:(fun line ->
         String.to_list line |> List.map ~f:(Char.( = ) '#') |> Array.of_list)
  |> Array.of_list

let init yb xb = Array.init yb ~f:(fun _ -> Array.create ~len:xb false)

let print (image : t) =
  Printf.printf "%s\n\n"
    (String.concat ~sep:"\n"
       (Array.map image ~f:(fun row ->
            String.concat
              (Array.map row ~f:(fun value -> if value then "#" else ".")
              |> Array.to_list))
       |> Array.to_list))

let bounds image =
  let ybound = Array.length image - 1 and xbound = Array.length image.(0) - 1 in
  (ybound, xbound)

let get image (y, x) =
  let ybound, xbound = bounds image in
  if y >= 0 && y <= ybound && x >= 0 && x <= xbound then Some image.(y).(x)
  else None

let get_exn image pos = Option.value_exn (get image pos)

let count image ~f =
  Array.sum
    (module Int)
    image
    ~f:(Array.sum (module Int) ~f:(fun value -> if f value then 1 else 0))

(* embed the given image in blank pixels *)
let surround (image : t) (value : bool) : t =
  let yb, xb = bounds image in
  Array.init (yb + 5) ~f:(fun y ->
      Array.init (xb + 5) ~f:(fun x ->
          get image (y - 2, x - 2) |> Option.value ~default:value))

(* trim off pixels added by surround *)
let unsurround (image : t) : t =
  let yb, xb = bounds image in
  Array.init (yb - 1) ~f:(fun y ->
      Array.init (xb - 1) ~f:(fun x -> get_exn image (y + 1, x + 1)))
