open Core
open Core.List.Let_syntax

type point = int * int * int [@@deriving sexp, compare]

let difference (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

let add (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

let range n = List.init n ~f:Fn.id

module PointMap = Map.Make (struct
  type t = point [@@deriving sexp, compare]
end)

let rotate_point p =
  let rotations (x, y, z) =
    [ (x, y, z); (x, z, y); (y, x, z); (y, z, x); (z, x, y); (z, y, x) ]
  in
  let flips (x, y, z) =
    [
      (x, y, z);
      (x, y, -z);
      (x, -y, -z);
      (x, -y, z);
      (-x, y, z);
      (-x, y, -z);
      (-x, -y, z);
      (-x, -y, -z);
    ]
  in
  rotations p |> List.concat_map ~f:flips

(* rotate_report returns a list of lists of possible points in each position of the report *)
let rotate_report = List.map ~f:rotate_point

(* optionally returns an offseted report which matches the first report *)
let match_reports : point list -> point list -> point list option =
 fun r1 r2 ->
  let r1 = rotate_report r1 and r2 = rotate_report r2 in

  let result = ref None in

  for rotated_offset = 0 to 47 do
    (* count how many (i, j) observations match under the given rotation offset *)
    if Option.is_some !result then ()
    else
      let offset_map = ref PointMap.empty in

      for i = 0 to List.length r1 - 1 do
        let p1 = List.nth_exn (List.nth_exn r1 i) 0 in
        for j = 0 to List.length r2 - 1 do
          let p2 = List.nth_exn (List.nth_exn r2 j) rotated_offset in

          let offset = difference p2 p1 in
          offset_map :=
            PointMap.update !offset_map offset ~f:(function
              | None -> 1
              | Some v -> v + 1)
        done
      done;

      if
        PointMap.filter !offset_map ~f:(fun v -> v >= 12) |> PointMap.length = 0
      then ()
      else
        let offset =
          PointMap.filter !offset_map ~f:(fun v -> v >= 12) |> PointMap.keys
          |> fun l -> List.nth_exn l 0
        in

        result :=
          Some
            (List.map r2 ~f:(fun point ->
                 difference (List.nth_exn point rotated_offset) offset))
  done;

  !result

let reports =
  let%map report =
    In_channel.read_lines "../input"
    |> List.group ~break:(fun _ a2 -> String.length a2 = 0)
  in

  let%map position =
    List.filter report ~f:(fun s ->
        String.length s > 0 && not (Char.( = ) (String.get s 1) '-'))
  in

  let position = String.split position ~on:',' |> List.map ~f:int_of_string in
  match position with
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith "invalid position"

let () =
  let resolved_reports = ref [ List.hd_exn reports ] in
  let reports = ref (List.tl_exn reports) in

  while List.length !reports > 0 do
    let match_index, matched_report =
      List.find_map_exn !resolved_reports ~f:(fun resolved_report ->
          List.find_mapi !reports ~f:(fun index report ->
              Option.map (match_reports resolved_report report)
                ~f:(fun report -> (index, report))))
    in

    resolved_reports := matched_report :: !resolved_reports;

    reports :=
      List.filteri !reports ~f:(fun index _ -> not (index = match_index))
  done;

  !resolved_reports |> List.concat
  |> List.dedup_and_sort ~compare:compare_point
  |> List.length |> Printf.printf "%d"
