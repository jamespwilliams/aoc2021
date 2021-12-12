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
  In_channel.read_lines "../input-test"
  |> List.map ~f:(fun s ->
         String.split ~on:'-' s |> List.map ~f:parse_colour |> tuple_of_list)

let neighbours graph node =
  List.filter graph ~f:(fun (f, t) ->
      colour_equal f node || colour_equal t node)
  |> List.map ~f:(fun (f, t) -> if colour_equal f node then t else f)

let count_paths graph =
  let is_seen seen = function
    | Start -> Set.mem seen "start"
    | Small s -> Set.mem seen s
    | _ -> false
  in

  let mark_seen seen = function
    | Start -> Set.add seen "start"
    | Small s -> Set.add seen s
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

  let seen = Set.empty (module String) in
  aux graph seen [] Start

let () = List.length (count_paths graph) |> string_of_int |> print_endline
