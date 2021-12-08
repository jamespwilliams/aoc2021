open Core

let tuple_of_list l = (List.nth_exn l 0, List.nth_exn l 1)

let parse_line line =
  String.split ~on:'|' line
  |> List.map ~f:(fun s ->
         String.split ~on:' ' s |> List.filter ~f:(fun s -> String.length s > 0))
  |> tuple_of_list

let () =
  let input = In_channel.read_lines "../input" |> List.map ~f:parse_line in

  let output_values = List.map ~f:snd input in

  List.fold output_values ~init:0 ~f:(fun acc values ->
      acc
      + List.count values ~f:(fun s ->
            let len = String.length s in
            len = 2 || len = 4 || len = 3 || len = 7))
  |> string_of_int |> print_endline
