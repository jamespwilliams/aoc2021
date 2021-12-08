open Core

let tuple_of_list l =
  let e = List.nth_exn l in
  (e 0, e 1)

let quadruple_of_list l =
  let e = List.nth_exn l in
  (e 0, e 1, e 2, e 3)

(* http://rosettacode.org/wiki/Permutations#OCaml *)
let rec permutations l =
  let n = List.length l in
  if n = 1 then [ l ]
  else
    let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t
    in
    let rec aux k =
      let e = List.nth_exn l k in
      let subperms = permutations (sub e l) in
      let t = List.map ~f:(fun a -> e :: a) subperms in
      if k < n - 1 then List.rev_append t (aux (k + 1)) else t
    in
    aux 0

let parse_line line =
  String.split ~on:'|' line
  |> List.map ~f:(fun s ->
         String.split ~on:' ' s
         |> List.filter ~f:(fun s -> String.length s > 0)
         |> List.map ~f:(fun s ->
                String.to_list s |> List.sort ~compare:Char.compare))
  |> tuple_of_list

let input = In_channel.read_lines "../input-test" |> List.map ~f:parse_line

let output_values =
  List.map
    ~f:(fun observation ->
      let _, out = observation in
      out)
    input

let input_values =
  List.map
    ~f:(fun observation ->
      let i, _ = observation in
      i)
    input

let apply_mapping mapping value =
  List.map value ~f:(fun c -> Hashtbl.find_exn mapping c)
  |> List.sort ~compare:Char.compare

let mapping_works mapping (one, four, seven) =
  let eq = List.equal Char.( = ) in
  eq (apply_mapping mapping one) [ 'c'; 'f' ]
  && eq (apply_mapping mapping four) [ 'b'; 'c'; 'd'; 'f' ]
  && eq (apply_mapping mapping seven) [ 'a'; 'c'; 'f' ]

let find_working_mapping input_values =
  let one, four, seven, _ =
    List.map [ 2; 4; 3; 7 ] ~f:(fun n ->
        Option.value_exn
          (List.find input_values ~f:(fun s -> List.length s = n)))
    |> quadruple_of_list
  in

  let segments = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ] in

  List.map
    (permutations (List.map ~f:int_of_char segments))
    ~f:(fun permutation ->
      List.map2_exn segments permutation ~f:(fun s p -> (s, char_of_int p)))
  |> List.map ~f:(Hashtbl.of_alist_exn (module Char))
  |> List.filter ~f:(fun m -> mapping_works m (one, four, seven))

let () =
  List.fold output_values ~init:0 ~f:(fun acc values ->
      acc
      + List.count values ~f:(fun s ->
            let len = List.length s in
            len = 2 || len = 4 || len = 3 || len = 7))
  |> string_of_int |> print_endline
