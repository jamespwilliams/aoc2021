open Core

type player = { position : int; score : int }

type state = {
  player_one : player;
  player_two : player;
  die : int;
  turns : int;
}

type turn = bool

type die = int

let startx, starty =
  List.(
    In_channel.read_lines "../input"
    >>| fun s -> String.split s ~on:' ' |> last_exn |> int_of_string)
  |> fun l -> (List.nth_exn l 0, List.nth_exn l 1)

let roll_die_thrice (die : die) : die * int =
  ((die + 3) % 100, 3 + die + ((die + 1) % 100) + ((die + 2) % 100))

let move (position : int) (die : die) : int * die =
  let die, move = roll_die_thrice die in
  ((move + position) % 10, die)

let step { position; score } die =
  let position, die = move position die in
  ({ position; score = score + position + 1 }, die)

let range n = List.init n ~f:Fn.id

let result =
  let initial_state =
    {
      player_one = { position = startx - 1; score = 0 };
      player_two = { position = starty - 1; score = 0 };
      die = 0;
      turns = 0;
    }
  in

  List.fold_until (range 10000) ~init:initial_state
    ~f:(fun { player_one; player_two; die; turns } _ ->
      let player_one, player_two, die =
        if turns % 2 = 0 then
          let player_one, die = step player_one die in
          (player_one, player_two, die)
        else
          let player_two, die = step player_two die in
          (player_one, player_two, die)
      in

      let turns = turns + 1 in

      if player_one.score >= 1000 || player_two.score >= 1000 then
        Stop (player_one.score, player_two.score, turns)
      else Continue { player_one; player_two; die; turns })
    ~finish:(fun _ -> failwith "iteration limit reached")

let () =
  let x, y, turns = result in
  List.min_elt ~compare [ x; y ]
  |> fun x -> Option.value_exn x |> ( * ) (3 * turns) |> Printf.printf "%d\n"
