open Core

let tuple_of_list l = (List.nth_exn l 0, List.nth_exn l 1)

let count_paths graph =
  let count_visits seen node =
    (match node with
    | Cave.Start -> Map.find seen "start"
    | Cave.Small s -> Map.find seen s
    | _ -> None)
    |> Option.value ~default:0
  in

  let is_seen seen node =
    match node with
    | Cave.Start -> count_visits seen node >= 1
    | Cave.Small _ ->
        let visits = count_visits seen node in
        if Map.data seen |> List.for_all ~f:(fun c -> c <= 1) then visits >= 2
        else visits >= 1
    | _ -> false
  in

  let mark_seen seen =
    let update o = match o with None -> 1 | Some v -> v + 1 in
    function
    | Cave.Start -> Map.update seen "start" ~f:update
    | Cave.Small s -> Map.update seen s ~f:update
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

  let seen = Map.empty (module String) in
  aux graph seen [] Start |> List.map ~f:List.rev

let () =
  In_channel.read_lines "../input"
  |> CaveGraph.graph_of_lines |> count_paths |> List.length
  |> Printf.printf "%d\n"
