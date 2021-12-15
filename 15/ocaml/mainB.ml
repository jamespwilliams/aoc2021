open Core

module Node = struct
  type t = int * int [@@deriving show, eq, ord, sexp_of, hash]
end

module Weighted_edge = struct
  type 'a t = int * 'a

  let compare (w1, _) (w2, _) = compare w1 w2
end

let graph_of_lines lines =
  List.map lines ~f:(fun list ->
      String.to_list list |> List.map ~f:Char.get_digit_exn |> Array.of_list)
  |> Array.of_list

let bounds graph =
  let ybound = Array.length graph - 1 and xbound = Array.length graph.(0) - 1 in
  (ybound, xbound)

let get graph (y, x) =
  let ybound, xbound = bounds graph in
  if y >= 0 && y <= ybound && x >= 0 && x <= xbound then Some graph.(y).(x)
  else None

let neighboursi graph (y, x) =
  List.map
    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
    ~f:(fun p -> (p, get graph p))
  |> List.filter ~f:(fun (_, o) -> Option.is_some o)
  |> List.map ~f:(fun (p, o) -> (p, Option.value_exn o))

let dijkstra graph startn endn =
  let pq = Pairing_heap.create ~cmp:Weighted_edge.compare () in
  Pairing_heap.add pq (0, startn);

  let distances = Hashtbl.create (module Node) in
  ignore @@ Hashtbl.add distances ~key:startn ~data:0;

  let find_distance distances node =
    Hashtbl.find distances node |> Option.value ~default:Int.max_value
  in

  while not (Pairing_heap.is_empty pq) do
    let _, u = Pairing_heap.pop_exn pq in

    List.iter
      ~f:(fun (v, w) ->
        let newdist = find_distance distances u + w in
        if newdist < find_distance distances v then (
          Pairing_heap.add pq (newdist, v);
          Hashtbl.set distances ~key:v ~data:newdist))
      (neighboursi graph u)
  done;

  Hashtbl.find_exn distances endn

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
  In_channel.read_lines "../input" |> graph_of_lines |> Fn.flip expand_graph 5

let print graph =
  Printf.printf "%s"
    (Array.fold ~init:"" graph ~f:(fun acc row ->
         Array.fold ~init:acc row ~f:(fun acc value ->
             acc ^ string_of_int value)
         ^ "\n"))

let () = dijkstra graph (0, 0) (bounds graph) |> Printf.printf "%d"
