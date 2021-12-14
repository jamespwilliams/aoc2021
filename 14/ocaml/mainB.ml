open Core

type pair = char * char

type pair_freq = pair * int

type rule = pair * char

let get_pair_frequencies template =
  let rec get_pairs : char list -> (char * char) list = function
    | a :: (b :: _ as rest) -> (a, b) :: get_pairs rest
    | _ -> []
  in

  get_pairs template
  |> List.sort ~compare:(Common.compare_tuple compare_char)
  |> Common.element_frequency ~compare:(Common.compare_tuple compare_char)

let find_substitution rules pattern =
  List.find_exn rules ~f:(fun rule ->
      Common.tuple_equals (fst rule) pattern Char.( = ))
  |> snd

let rec increment : int -> char * char -> pair_freq list -> pair_freq list =
 fun delta pair_to_increment pairs ->
  match pairs with
  | [] -> [ (pair_to_increment, delta) ]
  | (pair, count) :: l ->
      if Common.tuple_equals pair pair_to_increment Char.( = ) then
        (pair, count + delta) :: l
      else (pair, count) :: increment delta pair_to_increment l

let step : rule list -> pair_freq list -> pair_freq list =
 fun rules ->
  List.fold ~init:[] ~f:(fun acc_pair_freqs ((p1, p2), count) ->
      let sub = find_substitution rules (p1, p2) in
      increment count (sub, p2) (increment count (p1, sub) acc_pair_freqs))

let count_characters : pair_freq list -> (char * int) list =
 fun freq_pairs ->
  List.map freq_pairs ~f:(fun ((c, _), count) -> (c, count))
  |> List.sort ~compare:(fun (c1, _) (c2, _) -> compare_char c1 c2)
  |> List.group ~break:(fun (c1, _) (c2, _) -> not (compare_char c1 c2 = 0))
  |> List.map
       ~f:
         (List.fold ~init:('?', 0) ~f:(fun (_, acc_count) (c, count) ->
              (c, count + acc_count)))

let () =
  let polymer_template, pair_insertion_rules =
    In_channel.read_lines "../input" |> Common.parse_input
  in

  let starting_freqs = get_pair_frequencies polymer_template in

  List.fold (Common.range 40) ~init:starting_freqs ~f:(fun freqs _ ->
      step pair_insertion_rules freqs)
  |> count_characters |> List.map ~f:snd |> List.sort ~compare
  |> fun list ->
  Option.value_exn (List.max_elt ~compare list)
  - Option.value_exn (List.min_elt ~compare list)
  |> Printf.printf "%d"
