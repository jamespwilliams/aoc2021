open Core

(* let's call the players x and y respectively *)

type turn = bool

type die = int

module Quintuple = struct
  type t = int * int * int * int * int [@@deriving hash, sexp, compare]
end

module Quadruple = struct
  type t = int * int * int * int [@@deriving hash, sexp, compare]
end

let startx, starty =
  List.(
    In_channel.read_lines "../input"
    >>| fun s -> String.split s ~on:' ' |> last_exn |> int_of_string)
  |> fun l -> (List.nth_exn l 0, List.nth_exn l 1)

let possible_die_rolls =
  List.concat_map [ 1; 2; 3 ] ~f:(fun one ->
      List.concat_map [ 1; 2; 3 ] ~f:(fun two ->
          List.map [ 1; 2; 3 ] ~f:(fun three -> one + two + three)))

let rec find_number_of_wins x y x_score y_score turns cache =
  (* todo refactor *)
  match Hashtbl.find cache (x, y, x_score, y_score, turns % 2) with
  | Some result -> result
  | None ->
      let result =
        if x_score >= 21 then (1, 0)
        else if y_score >= 21 then (0, 1)
        else if turns % 2 = 0 then
          List.map possible_die_rolls ~f:(fun die_roll ->
              let next_x = (x + die_roll) % 10 in
              find_number_of_wins next_x y
                (x_score + next_x + 1)
                y_score (turns + 1) cache)
          |> List.fold ~init:(0, 0) ~f:(fun (x1, y1) (x2, y2) ->
                 (x1 + x2, y1 + y2))
        else
          List.map possible_die_rolls ~f:(fun die_roll ->
              let next_y = (y + die_roll) % 10 in
              find_number_of_wins x next_y x_score
                (y_score + next_y + 1)
                (turns + 1) cache)
          |> List.fold ~init:(0, 0) ~f:(fun (x1, y1) (x2, y2) ->
                 (x1 + x2, y1 + y2))
      in
      Hashtbl.add_exn cache
        ~key:(x, y, x_score, y_score, turns % 2)
        ~data:result;
      result

let () =
  let xwins, ywins =
    find_number_of_wins (startx - 1) (starty - 1) 0 0 0
      (Hashtbl.create (module Quintuple))
  in
  List.max_elt [ xwins; ywins ] ~compare
  |> fun x -> Option.value_exn x |> Printf.printf "%d\n"
