open Core

type bingo_number = { mutable marked : bool; number : int }

type t = bingo_number Array.t Array.t

let wins card =
  let row_wins card =
    Array.exists card ~f:(fun row ->
        Array.for_all row ~f:(fun num -> num.marked))
  in
  row_wins card || row_wins (Array.transpose_exn card)

let iter card ~f = Array.iter card ~f:(fun row -> Array.iter row ~f)

let mark : t -> int -> unit =
 fun card set_num ->
  iter card ~f:(fun num -> if num.number = set_num then num.marked <- true)

let score : t -> int =
 fun card ->
  Array.fold card ~init:0 ~f:(fun acc row ->
      acc
      + Array.fold row ~init:0 ~f:(fun acc entry ->
            acc + if entry.marked then 0 else entry.number))
