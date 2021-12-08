open Core

let find_winning_card cards numbers =
  let rec play_bingo_number number = function
    | card :: cards ->
        Card.mark card number;
        if Card.wins card then Some card else play_bingo_number number cards
    | [] -> None
  in
  List.map ~f:(fun n -> (n, play_bingo_number n cards)) numbers
  |> List.find ~f:(fun (_, card) -> is_some card)

let () =
  let input = In_channel.read_lines "../input" in

  let cards, bingo_numbers = Parse.parse_input input in

  let winning_number, winning_card =
    Option.value_exn (find_winning_card cards bingo_numbers)
  in

  print_endline @@ string_of_int
  @@ (Card.score (Option.value_exn winning_card) * winning_number)
