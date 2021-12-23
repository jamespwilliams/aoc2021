open! Core

type light_state = On | Off

(* TODO: use a record here? *)
type cube = (int * int) * (int * int) * (int * int)

type instruction = light_state * cube

let range_of_string s =
  let _, s = String.lsplit2_exn s ~on:'=' in
  let start, end_ = String.lsplit2_exn s ~on:'.' in
  let _, end_ = String.lsplit2_exn end_ ~on:'.' in
  (int_of_string start, int_of_string end_ + 1)

let cube_of_string s =
  let components = String.split ~on:',' s |> List.map ~f:range_of_string in
  match components with
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "invalid cube specification"

let instruction_of_string s =
  let kind, rest = String.lsplit2_exn s ~on:' ' in
  match kind with
  | "on" -> (On, cube_of_string rest)
  | "off" -> (Off, cube_of_string rest)
  | _ -> failwith "invalid instruction"

let parse_input lines = lines |> List.map ~f:instruction_of_string
