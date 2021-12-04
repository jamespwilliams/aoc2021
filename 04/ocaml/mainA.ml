open Core

type bingo_number = { mutable marked : bool; number : int }

type card = bingo_number Array.t Array.t

let card_wins card =
  let row_wins card =
    Array.exists card ~f:(fun row ->
        Array.for_all row ~f:(fun num -> num.marked))
  in
  row_wins card || row_wins (Array.transpose_exn card)

let card_iter card ~f = Array.iter card ~f:(fun row -> Array.iter row ~f)

let card_mark : card -> int -> unit =
 fun card set_num ->
  card_iter card ~f:(fun num -> if num.number = set_num then num.marked <- true)

let card_score : card -> int =
 fun card ->
  Array.fold card ~init:0 ~f:(fun acc row ->
      acc
      + Array.fold row ~init:0 ~f:(fun acc entry ->
            acc + if entry.marked then 0 else entry.number))

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

let find_winning_card cards numbers =
  let rec play_bingo_number number = function
    | card :: cards ->
        card_mark card number;
        if card_wins card then Some card else play_bingo_number number cards
    | [] -> None
  in
  List.map ~f:(fun n -> (n, play_bingo_number n cards)) numbers
  |> List.find ~f:(fun (_, card) -> is_some card)

let () =
  let input = In_channel.read_lines "../input" in

  let bingo_numbers_line = List.hd_exn input in

  let cards_lines = List.tl_exn @@ List.tl_exn input in

  let bingo_numbers =
    String.split bingo_numbers_line ~on:',' |> List.map ~f:int_of_string
  in

  let cards = parse_cards cards_lines in

  let winning_number, winning_card =
    Option.value_exn (find_winning_card cards bingo_numbers)
  in
  print_endline @@ string_of_int
  @@ (card_score (Option.value_exn winning_card) * winning_number)
