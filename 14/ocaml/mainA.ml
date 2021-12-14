open Core

let apply_rules : char * char -> ((char * char) * char) list -> char list =
 fun ((a, b) as pattern) rules ->
  let _, substitution =
    List.find_exn rules ~f:(fun rule ->
        Common.tuple_equals (fst rule) pattern Char.( = ))
  in
  [ a; substitution; b ]

let rec step : ((char * char) * char) list -> char list -> char list =
 fun rules template ->
  match template with
  | ([] | [ _ ]) as template -> template
  | a :: b :: template ->
      let substituted = apply_rules (a, b) rules in
      List.slice substituted 0 (-1) @ step rules (b :: template)

let () =
  let polymer_template, pair_insertion_rules =
    In_channel.read_lines "../input" |> Common.parse_input
  in
  List.fold (Common.range 10) ~init:polymer_template ~f:(fun template _ ->
      step pair_insertion_rules template)
  |> List.sort ~compare:compare_char
  |> Common.element_frequency ~compare:compare_char
  |> List.map ~f:snd
  |> fun list ->
  Option.value_exn (List.max_elt ~compare list)
  - Option.value_exn (List.min_elt ~compare list)
  |> Printf.printf "%d"
