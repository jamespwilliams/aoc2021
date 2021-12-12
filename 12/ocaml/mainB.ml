open Core

type colour = Start | End | Big of string | Small of string

let colour_equal c1 c2 =
  match (c1, c2) with
  | Start, Start -> true
  | End, End -> true
  | Big s1, Big s2 -> String.( = ) s1 s2
  | Small s1, Small s2 -> String.( = ) s1 s2
  | _ -> false

let is_lowercase = String.for_all ~f:Char.is_lowercase

let tuple_of_list l = (List.nth_exn l 0, List.nth_exn l 1)

let parse_colour = function
  | "start" -> Start
  | "end" -> End
  | s -> if is_lowercase s then Small s else Big s

let pp_colour = function
  | Start -> "start"
  | End -> "end"
  | Big s -> s
  | Small s -> s

let graph =
  In_channel.read_lines "../input"
  |> List.map ~f:(fun s ->
         String.split ~on:'-' s |> List.map ~f:parse_colour |> tuple_of_list)

let neighbours graph node =
  List.filter graph ~f:(fun (f, t) ->
      colour_equal f node || colour_equal t node)
  |> List.map ~f:(fun (f, t) -> if colour_equal f node then t else f)

let count_paths graph =
  let count_visits seen node =
    (match node with
    | Start -> Map.find seen "start"
    | Small s -> Map.find seen s
    | _ -> None)
    |> Option.value ~default:0
  in

  let is_seen seen node =
    match node with
    | Start -> count_visits seen node >= 1
    | Small _ ->
        let visits = count_visits seen node in
        if Map.data seen |> List.for_all ~f:(fun c -> c <= 1) then visits >= 2
        else visits >= 1
    | _ -> false
  in

  let mark_seen seen =
    let update o = match o with None -> 1 | Some v -> v + 1 in
    function
    | Start -> Map.update seen "start" ~f:update
    | Small s -> Map.update seen s ~f:update
    | _ -> seen
  in

  let rec aux graph seen path node =
    match node with
    | End -> [ End :: path ]
    | node ->
        if is_seen seen node then []
        else
          let seen = mark_seen seen node in
          neighbours graph node
          |> List.concat_map ~f:(aux graph seen (node :: path))
  in

  let seen = Map.empty (module String) in
  aux graph seen [] Start |> List.map ~f:List.rev

let () = count_paths graph |> List.length |> Printf.printf "%d\n"
