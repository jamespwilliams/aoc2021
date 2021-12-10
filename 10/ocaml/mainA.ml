open Core

let input = In_channel.read_lines "../input" |> List.map ~f:String.to_list

let is_closer = function
  | '}' -> true
  | ']' -> true
  | ')' -> true
  | '>' -> true
  | _ -> false

let score_error = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> failwith "invalid character"

let closer = function
  | '{' -> '}'
  | '[' -> ']'
  | '(' -> ')'
  | '<' -> '>'
  | _ -> failwith "invalid character"

let find_syntax_errors : char list -> char list =
 fun line ->
  let rec aux stack line =
    match line with
    | char :: chars ->
        if is_closer char then
          let expected_char = Stack.pop_exn stack in
          if Char.( = ) char expected_char then aux stack chars
          else char :: aux stack chars
        else (
          Stack.push stack (closer char);
          aux stack chars)
    | _ -> []
  in
  aux (Stack.create ()) line

let score line =
  find_syntax_errors line |> List.hd
  |> Option.value_map ~default:0 ~f:(fun char -> score_error char)

let () =
  input
  |> List.sum (module Int) ~f:(fun x -> score x)
  |> string_of_int |> print_endline
