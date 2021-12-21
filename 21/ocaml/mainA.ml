open Core

(* let's call the players x and y respectively *)

type turn = bool

type die = int

let startx, starty =
  List.(
    In_channel.read_lines "../input"
    >>| fun s -> String.split s ~on:' ' |> last_exn |> int_of_string)
  |> fun l -> (List.nth_exn l 0, List.nth_exn l 1)

let increment_die (die : die) (times : int) : die * int =
  let times = ref times in
  let result = ref 0 in
  let die = ref die in
  while !times > 0 do
    result := !result + !die;
    times := !times - 1;
    die :=
      let next = !die + 1 in
      if next > 100 then 1 else next
  done;
  (!die, !result)

let rec move (position : int) (die : die) : int * die =
  let die, move = increment_die die 3 in
  ((move + position) % 10, die)

let step position score die =
  let position, die = move position die in
  (position, score + (position + 1), die)

let range n = List.init n ~f:Fn.id

let result =
  List.fold_until (range 10000)
    ~init:(startx - 1, starty - 1, 0, 0, 1, 0)
    ~f:(fun (x, y, x_score, y_score, die, turns) _ ->
      Printf.printf "%d %d (%d %d)\n" x y x_score y_score;

      let x, y, x_score, y_score, die =
        if turns % 2 = 0 then
          let x, x_score, die = step x x_score die in
          (x, y, x_score, y_score, die)
        else
          let y, y_score, die = step y y_score die in
          (x, y, x_score, y_score, die)
      in

      let turns = turns + 1 in

      Printf.printf "%d %d (%d %d)\n" x y x_score y_score;

      if x_score >= 1000 || y_score >= 1000 then Stop (x_score, y_score, turns)
      else Continue (x, y, x_score, y_score, die, turns))
    ~finish:(fun _ -> failwith "iteration limit reached")

let () =
  let x, y, turns = result in
  List.min_elt ~compare [ x; y ]
  |> fun x -> Option.value_exn x |> ( * ) (3 * turns) |> Printf.printf "%d\n"
