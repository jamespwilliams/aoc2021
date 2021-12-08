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

let sum m card ~f = Array.sum m card ~f:(fun row -> Array.sum m row ~f)

let mark : t -> int -> unit =
 fun card set_num ->
  iter card ~f:(fun num -> if num.number = set_num then num.marked <- true)

let score : t -> int =
 fun card ->
  sum
    (module Int)
    card
    ~f:(fun entry -> if entry.marked then 0 else entry.number)
