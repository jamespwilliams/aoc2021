open Core

let tuple_of_list l =
  let e = List.nth_exn l in
  (e 0, e 1)

let quadruple_of_list l =
  let e = List.nth_exn l in
  (e 0, e 1, e 2, e 3)

let parse_line line =
  String.split ~on:'|' line
  |> List.map ~f:(fun s ->
         String.split ~on:' ' s
         |> List.filter ~f:(fun s -> String.length s > 0)
         |> List.map ~f:(fun s ->
                String.to_list s |> List.sort ~compare:Char.compare))
  |> tuple_of_list

let segment_subset digit ~of_ =
  let digit1_components, digit2_components =
    (Set.of_list (module Char) digit, Set.of_list (module Char) of_)
  in
  Set.is_subset digit1_components ~of_:digit2_components

let find_mapping input_values =
  let find_value ~length ?(subset_of = []) ?(superset_of = [])
      ?(not_equal_to = []) () =
    List.find_exn input_values ~f:(fun s ->
        List.length s = length
        && List.for_all subset_of ~f:(fun v -> segment_subset s ~of_:v)
        && List.for_all superset_of ~f:(fun v -> segment_subset v ~of_:s)
        && List.for_all not_equal_to ~f:(fun v ->
               not (List.equal Char.( = ) s v)))
  in

  let one, four, seven, eight =
    List.map [ 2; 4; 3; 7 ] ~f:(fun n -> find_value ~length:n ())
    |> quadruple_of_list
  in

  let three = find_value ~length:5 ~superset_of:[ one ] () in
  let nine = find_value ~length:6 ~superset_of:[ four ] () in
  let zero =
    find_value ~length:6 ~superset_of:[ seven ] ~not_equal_to:[ nine ] ()
  in
  let five =
    find_value ~length:5 ~subset_of:[ nine ] ~not_equal_to:[ three ] ()
  in
  let two = find_value ~length:5 ~not_equal_to:[ three; five ] () in
  let six = find_value ~length:6 ~not_equal_to:[ nine; zero ] () in

  [ zero; one; two; three; four; five; six; seven; eight; nine ]

let get_mapped_value mapping value =
  Option.value_exn
    (List.findi mapping ~f:(fun _ m -> List.equal Char.( = ) value m))
  |> fst

let get_mapped_number mapping number =
  List.fold number ~init:0 ~f:(fun acc value ->
      get_mapped_value mapping value + (10 * acc))

let () =
  let input = In_channel.read_lines "../input" |> List.map ~f:parse_line in

  List.fold input ~init:0 ~f:(fun acc (input_values, output_values) ->
      let mapping = find_mapping input_values in
      acc + get_mapped_number mapping output_values)
  |> string_of_int |> print_endline
