open Core

let tuple_equal (a, b) (c, d) = a = c && b = d

let input = In_channel.read_lines "../input" |> Seafloor.from_lines

let rec floodfill seafloor position queue =
  if not (Queue.exists queue ~f:(tuple_equal position)) then []
  else (
    Queue.filter_inplace queue ~f:(fun p -> not (tuple_equal p position));

    if Seafloor.get_exn seafloor position = 9 then []
    else
      position
      ::
      (Seafloor.neighboursi seafloor position
      |> List.map ~f:fst
      |> List.concat_map ~f:(fun (y, x) -> floodfill seafloor (y, x) queue)))

let floodfill_all seafloor =
  let queue = Queue.create () in
  Seafloor.iteri seafloor ~f:(fun (iy, ix) _ -> Queue.enqueue queue (iy, ix));
  let rs = ref [] in
  while not (Queue.is_empty queue) do
    let position = Option.value_exn (Queue.peek queue) in

    let region = floodfill seafloor position queue in
    rs := region :: !rs
  done;
  !rs |> List.filter ~f:(fun l -> List.length l > 0)

let () =
  floodfill_all input |> List.map ~f:List.length
  |> List.filter ~f:(fun l -> l > 0)
  |> List.sort ~compare:(fun a b -> compare b a)
  |> List.filteri ~f:(fun index _ -> index < 3)
  |> List.fold ~init:1 ~f:( * ) |> string_of_int |> print_endline
