open Core

type cuboid = (int * int) * (int * int) * (int * int)

type light_state = On | Off

type instruction = light_state * cuboid

let range_of_string s =
  let _, s = String.lsplit2_exn s ~on:'=' in
  let start, end_ = String.lsplit2_exn s ~on:'.' in
  let _, end_ = String.lsplit2_exn end_ ~on:'.' in
  (int_of_string start, int_of_string end_)

let cuboid_of_string s =
  let components = String.split ~on:',' s |> List.map ~f:range_of_string in
  match components with
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "invalid cuboid specification"

let instruction_of_string s =
  let kind, rest = String.lsplit2_exn s ~on:' ' in
  match kind with
  | "on" -> (On, cuboid_of_string rest)
  | "off" -> (Off, cuboid_of_string rest)
  | _ -> failwith "invalid instruction"

let in_bounds x = x <= 50 && x >= -50

let parse_input lines =
  lines
  |> List.map ~f:instruction_of_string
  |> List.filter ~f:(function _, c ->
         let (x1, x2), (y1, y2), (z1, z2) = c in
         ((in_bounds x1 || in_bounds x2) && (in_bounds y1 || in_bounds y2))
         || in_bounds z1 || in_bounds z2)

let () =
  let input = In_channel.read_lines "../input" |> parse_input in

  let cube =
    Array.init 101 ~f:(fun _ ->
        Array.init 101 ~f:(fun _ -> Array.create ~len:101 false))
  in

  let apply_instruction cube (kind, ((x1, x2), (y1, y2), (z1, z2))) =
    for x = x1 to x2 do
      for y = y1 to y2 do
        for z = z1 to z2 do
          if in_bounds x && in_bounds y && in_bounds z then
            cube.(x + 50).(y + 50).(z + 50) <-
              (match kind with On -> true | Off -> false)
        done
      done
    done
  in

  List.iter input ~f:(apply_instruction cube);

  let sum a ~f = Array.sum (module Int) a ~f in

  sum cube ~f:(sum ~f:(sum ~f:(fun x -> if x then 1 else 0)))
  |> Printf.printf "%d\n"
