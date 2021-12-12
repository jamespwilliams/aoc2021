open Core

let tuple_of_list l = (List.nth_exn l 0, List.nth_exn l 1)

let graph_of_lines =
  List.map ~f:(fun s ->
      String.split ~on:'-' s |> List.map ~f:Cave.cave_of_string |> tuple_of_list)

let neighbours graph node =
  List.filter graph ~f:(fun (f, t) -> Cave.equal f node || Cave.equal t node)
  |> List.map ~f:(fun (f, t) -> if Cave.equal f node then t else f)
