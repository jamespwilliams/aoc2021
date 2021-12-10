open Core

let input = In_channel.read_lines "../input" |> List.map ~f:String.to_list

let is_closer = function
  | '}' -> true
  | ']' -> true
  | ')' -> true
  | '>' -> true
  | _ -> false

let score_completion_char = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> failwith "invalid character"

let score_completion chars =
  List.fold chars ~init:0 ~f:(fun acc char ->
      (acc * 5) + score_completion_char char)

let closer = function
  | '{' -> '}'
  | '[' -> ']'
  | '(' -> ')'
  | '<' -> '>'
  | _ -> failwith "invalid character"

let find_completion : char list -> char list option =
 fun line ->
  let rec aux stack line =
    match line with
    | char :: chars ->
        if is_closer char then
          let expected_char = Stack.pop_exn stack in
          if Char.( = ) char expected_char then aux stack chars else None
        else (
          Stack.push stack (closer char);
          aux stack chars)
    | _ -> Some (Stack.to_list stack)
  in
  aux (Stack.create ()) line

let score line =
  find_completion line
  |> Option.value_map ~default:None ~f:(fun line ->
         Some (score_completion line))

let () =
  input
  |> List.map ~f:(fun x -> score x)
  |> List.filter ~f:Option.is_some
  |> List.map ~f:(fun x -> Option.value_exn x)
  |> List.sort ~compare
  |> fun l ->
  List.nth_exn l (List.length l / 2) |> string_of_int |> print_endline
