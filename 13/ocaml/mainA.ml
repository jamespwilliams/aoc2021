open Core

let tuple_of_list l = (List.nth_exn l 0, List.nth_exn l 1)

let compare_tuple (x1, y1) (x2, y2) =
  if x1 = x2 then compare y1 y2 else compare x1 x2

type fold = Up of int | Left of int

let fold_of_string s =
  let direction, count_str = String.lsplit2_exn ~on:'=' s in
  let count = int_of_string count_str in
  match direction with
  | "x" -> Left count
  | "y" -> Up count
  | _ -> failwith "invalid fold specification"

let points, folds =
  let point_lines, fold_lines =
    In_channel.read_lines "../input"
    |> List.split_while ~f:(fun x -> String.length x > 0)
  in
  let fold_lines = List.tl_exn fold_lines in
  ( point_lines
    |> List.map ~f:(fun line ->
           String.split ~on:',' line |> List.map ~f:int_of_string
           |> tuple_of_list),
    fold_lines
    |> List.map ~f:(fun line ->
           String.split ~on:' ' line |> fun l ->
           List.nth_exn l 2 |> fold_of_string) )

let reflect fold (x, y) =
  match fold with
  | Up i -> if y < i then (x, y) else (x, i - abs (i - y))
  | Left i -> if x < i then (x, y) else (i - abs (i - x), y)

let do_fold fold points =
  List.map ~f:(reflect fold) points
  |> List.dedup_and_sort ~compare:compare_tuple

let () =
  do_fold (List.nth_exn folds 0) points |> List.length |> Printf.printf "%d"
