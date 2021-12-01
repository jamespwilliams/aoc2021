open Core

let slurp file = In_channel.read_lines file

let input = List.map ~f:int_of_string (slurp "../input.txt")

let rec countIncreases list = match list with
  | a::(b::_ as tail) -> (if (a < b) then 1 else 0) + countIncreases (tail)
  | _                 -> 0

let () = print_endline (string_of_int (countIncreases input))
