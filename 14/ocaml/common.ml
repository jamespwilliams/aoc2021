open Core

let tuple_of_list = function [ a; b ] -> (a, b) | _ -> failwith "invalid list"

let list_of_tuple (a, b) = [ a; b ]

let compare_tuple compare (a1, b1) (a2, b2) =
  if compare a1 a2 = 0 then compare b1 b2 else compare a1 a2

let tuple_equals (a1, b1) (a2, b2) eq = eq a1 a2 && eq b1 b2

let range n = List.init n ~f:(fun x -> x)

let parse_insertion_rule rule =
  let components = String.split ~on:' ' rule in
  let predicate = List.nth_exn components 0 in
  ( String.to_list predicate |> tuple_of_list,
    String.get (List.nth_exn components 2) 0 )

let parse_input lines =
  let polymer_template, pair_insertion_lines =
    lines |> List.split_while ~f:(fun line -> String.length line > 0)
  in
  let pair_insertion_lines = List.tl_exn pair_insertion_lines in
  ( String.to_list (List.nth_exn polymer_template 0),
    List.map ~f:parse_insertion_rule pair_insertion_lines )

(* element_frequency expects a sorted list *)
let element_frequency : 'a list -> compare:('a -> 'a -> int) -> ('a * int) list
    =
 fun l ~compare ->
  let rec aux l current_elem current_count =
    match l with
    | a :: l ->
        if compare a current_elem = 0 then aux l current_elem (current_count + 1)
        else (current_elem, current_count) :: aux l a 1
    | [] -> [ (current_elem, current_count) ]
  in
  aux l (List.nth_exn l 0) 0
