open Core

let count_paths graph =
  let is_seen seen = function
    | (Cave.Start | Cave.Small _) as node -> Set.mem seen node
    | _ -> false
  in

  let mark_seen seen = function
    | (Cave.Start | Cave.Small _) as node -> Set.add seen node
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

  let seen = Set.empty (module Cave) in
  aux graph seen [] Cave.Start

let () =
  In_channel.read_lines "../input"
  |> CaveGraph.graph_of_lines |> count_paths |> List.length |> string_of_int
  |> print_endline
