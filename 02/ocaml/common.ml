open Core
open Core.String

type direction = Up | Forward | Down

let direction_of_string = function
  | "forward" -> Some Forward
  | "up" -> Some Up
  | "down" -> Some Down
  | _ -> None

type command = direction * int

let command_of_string_opt str =
  let command_opt dir_opt count_opt =
    Option.map2 dir_opt count_opt ~f:(fun dir count -> (dir, count))
  in

  match split ~on:' ' str with
  | [ dir_str; count_str ] ->
      command_opt (direction_of_string dir_str) (int_of_string_opt count_str)
  | _ -> None

let read_commands : string -> command list =
 fun filename ->
  In_channel.read_lines filename
  |> List.map ~f:command_of_string_opt
  |> List.map ~f:(fun x -> Option.value_exn x)
