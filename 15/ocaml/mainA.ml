open Core

let graph = In_channel.read_lines "../input" |> Common.graph_of_lines

let () = Common.(dijkstra graph (0, 0) (bounds graph)) |> Printf.printf "%d"
