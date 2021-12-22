open Core

type player = { position : int; score : int } [@@deriving hash, sexp, compare]

module State = struct
  type t = { player_one : player; player_two : player }
  [@@deriving hash, sexp, compare]

  let create player_one_start player_two_start =
    {
      player_one = { position = player_one_start; score = 0 };
      player_two = { position = player_two_start; score = 0 };
    }
end

let startx, starty =
  List.(
    In_channel.read_lines "../input"
    >>| fun s -> String.split s ~on:' ' |> last_exn |> int_of_string)
  |> fun l -> (List.nth_exn l 0 - 1, List.nth_exn l 1 - 1)

let possible_die_rolls =
  List.concat_map [ 1; 2; 3 ] ~f:(fun one ->
      List.concat_map [ 1; 2; 3 ] ~f:(fun two ->
          List.map [ 1; 2; 3 ] ~f:(fun three -> one + two + three)))

let rec find_number_of_wins (state : State.t) cache =
  if state.player_one.score >= 21 then (1, 0)
  else if state.player_two.score >= 21 then (0, 1)
  else
    match Hashtbl.find cache state with
    | Some result -> result
    | None ->
        let result =
          List.map possible_die_rolls ~f:(fun die_roll ->
              let position = (state.player_one.position + die_roll) % 10 in
              let score = state.player_one.score + position + 1 in

              find_number_of_wins
                {
                  player_one = state.player_two;
                  player_two = { position; score };
                }
                cache)
          |> List.fold ~init:(0, 0) ~f:(fun (x1, y1) (x2, y2) ->
                 (x1 + y2, y1 + x2))
        in
        Hashtbl.add_exn cache ~key:state ~data:result;
        result

let () =
  let xwins, ywins =
    find_number_of_wins
      (State.create startx starty)
      (Hashtbl.create (module State))
  in
  List.max_elt [ xwins; ywins ] ~compare
  |> fun x -> Option.value_exn x |> Printf.printf "%d\n"
