open Core
open Card

let parse_cards cards_lines =
  let new_card _ =
    Array.create ~len:0 (Array.create ~len:0 { marked = false; number = 0 })
  in

  let first_card = new_card () in

  let parse_card_row : string -> bingo_number Array.t Array.t =
   fun line ->
    String.split ~on:' ' line
    |> List.filter ~f:(fun s -> String.length s > 0)
    |> List.map ~f:int_of_string
    |> List.map ~f:(fun n -> { marked = false; number = n })
    |> Array.of_list
    |> fun x -> Array.of_list [ x ]
  in

  let rec parse_cards_aux this_card cards = function
    | "" :: lines -> parse_cards_aux (new_card ()) (this_card :: cards) lines
    | line :: lines ->
        parse_cards_aux
          (Array.append this_card (parse_card_row line))
          cards lines
    | [] -> this_card :: cards
  in

  parse_cards_aux first_card [] cards_lines

let parse_input lines =
  let bingo_numbers_line = List.hd_exn lines in

  let cards_lines = List.tl_exn @@ List.tl_exn lines in

  let bingo_numbers =
    String.split bingo_numbers_line ~on:',' |> List.map ~f:int_of_string
  in

  let cards = parse_cards cards_lines in
  (cards, bingo_numbers)
