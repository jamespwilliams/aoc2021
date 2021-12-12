open Core

let count_paths graph =
  let is_seen seen = function
    | Cave.Start -> Set.mem seen "start"
    | Cave.Small s -> Set.mem seen s
    | _ -> false
  in

  let mark_seen seen = function
    | Cave.Start -> Set.add seen "start"
    | Cave.Small s -> Set.add seen s
    | _ -> seen
  in

  let rec aux graph seen path node =
    match node with
    | Cave.End -> [ Cave.End :: path ]
    | node ->
        if is_seen seen node then []
        else
          let seen = mark_seen seen node in
          CaveGraph.neighbours graph node
          |> List.concat_map ~f:(aux graph seen (node :: path))
  in

  let seen = Set.empty (module String) in
  aux graph seen [] Cave.Start

let () =
  In_channel.read_lines "../input"
  |> CaveGraph.graph_of_lines |> count_paths |> List.length |> string_of_int
  |> print_endline
