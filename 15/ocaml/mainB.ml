open Core

let range n = List.init n ~f:Fn.id

let expand_graph graph times =
  let add_and_wrap a b =
    let res = a + b in
    (* bit of a hack here, because we know that the count won't ever go over 20: *)
    if res > 9 then a + b - 9 else res
  in
  let expanded_rows =
    Array.map graph ~f:(fun row ->
        Array.concat
          (List.map (range times) ~f:(fun n ->
               Array.map row ~f:(add_and_wrap n))))
  in
  Array.concat
    (List.map (range times) ~f:(fun n ->
         Array.map expanded_rows ~f:(Array.map ~f:(add_and_wrap n))))

let graph =
  In_channel.read_lines "../input"
  |> Common.graph_of_lines |> Fn.flip expand_graph 5

let () = Common.(dijkstra graph (0, 0) (bounds graph)) |> Printf.printf "%d"
