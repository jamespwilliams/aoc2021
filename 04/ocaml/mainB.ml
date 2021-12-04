open Core
open Common

let winning_cards cards numbers =
  let rec winning_cards_aux number = function
    | card :: cards ->
        Card.mark card number;
        let winners, losers = winning_cards_aux number cards in
        if Card.wins card then (card :: winners, losers)
        else (winners, card :: losers)
    | [] -> ([], [])
  in
  List.folding_map numbers ~init:cards ~f:(fun cards n ->
      let winners, losers = winning_cards_aux n cards in
      (losers, (n, winners)))

let last_winning_card_and_number cards numbers =
  let rev_results = List.rev (winning_cards cards numbers) in
  let last_win_number, last_winners =
    Option.value_exn
      (List.find rev_results ~f:(fun (_, winners) -> List.length winners > 0))
  in
  (last_win_number, List.hd_exn last_winners)

let () =
  let input = In_channel.read_lines "../input" in

  let cards, bingo_numbers = parse_input input in

  let winning_number, winning_card =
    last_winning_card_and_number cards bingo_numbers
  in
  print_endline @@ string_of_int @@ (Card.score winning_card * winning_number)
