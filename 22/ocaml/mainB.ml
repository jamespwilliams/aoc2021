open Core
open Common

let contains ((ax1, ax2), (ay1, ay2), (az1, az2))
    ((bx1, bx2), (by1, by2), (bz1, bz2)) =
  ax1 <= bx1 && ax2 >= bx2 && ay1 <= by1 && ay2 >= by2 && az1 <= bz1
  && az2 >= bz2

let intersects ((ax1, ax2), (ay1, ay2), (az1, az2))
    ((bx1, bx2), (by1, by2), (bz1, bz2)) =
  not
    (ax1 > bx2 || ax2 < bx1 || ay1 > by2 || ay2 < by1 || az1 > bz2 || az2 < bz1)

let volume ((x1, x2), (y1, y2), (z1, z2)) =
  List.fold ~init:1 ~f:( * ) [ x2 - x1; y2 - y1; z2 - z1 ]

let rec map_pairwise ~f = function
  | a :: (b :: _ as rest) -> f a b :: map_pairwise ~f rest
  | _ -> []

let concat_map_pairwise ~f list = List.concat @@ map_pairwise ~f list

let subtract_intersection ~(from : cube) ~(cube : cube) : cube list =
  (* takes a cube (~from) and subtracts another cube (~cube) from it, returning the resulting cube
     subcomponents, if any remain *)
  let (ax1, ax2), (ay1, ay2), (az1, az2) = from in
  let (bx1, bx2), (by1, by2), (bz1, bz2) = cube in

  if not (intersects from cube) then [ from ]
  else
    let x_splits = List.filter [ bx1; bx2 ] ~f:(fun x -> ax1 < x && x < ax2) in
    let y_splits = List.filter [ by1; by2 ] ~f:(fun y -> ay1 < y && y < ay2) in
    let z_splits = List.filter [ bz1; bz2 ] ~f:(fun z -> az1 < z && z < az2) in

    let x_vertices = [ ax1 ] @ x_splits @ [ ax2 ] in
    let y_vertices = [ ay1 ] @ y_splits @ [ ay2 ] in
    let z_vertices = [ az1 ] @ z_splits @ [ az2 ] in

    concat_map_pairwise x_vertices ~f:(fun x1 x2 ->
        concat_map_pairwise y_vertices ~f:(fun y1 y2 ->
            map_pairwise z_vertices ~f:(fun z1 z2 ->
                ((x1, x2), (y1, y2), (z1, z2)))))
    |> List.filter ~f:(fun c -> not (contains cube c))

let () =
  let input = In_channel.read_lines "../input" |> parse_input in

  let cubes =
    List.fold input ~init:[] ~f:(fun cubes instruction ->
        let kind, cube = instruction in
        let next_cubes =
          List.concat_map cubes ~f:(fun from ->
              subtract_intersection ~from ~cube)
        in
        match kind with On -> cube :: next_cubes | Off -> next_cubes)
  in

  List.sum (module Int) cubes ~f:volume |> Printf.printf "%d\n"
