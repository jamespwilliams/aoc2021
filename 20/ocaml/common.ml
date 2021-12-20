open Core

let parse_algorithm (algo : string) : bool list =
  String.to_list algo |> List.map ~f:(Char.( = ) '#')

let int_of_bitstring (bitstring : bool list) : int =
  List.fold bitstring ~init:0 ~f:(fun acc v -> (2 * acc) + if v then 1 else 0)

let parse_input lines =
  match lines with
  | algorithm :: "" :: image ->
      (parse_algorithm algorithm, Image.from_lines image)
  | _ -> failwith "invalid input"

let square_bitstring (image : Image.t) ((y, x) : int * int) : bool list =
  [
    (y - 1, x - 1);
    (y - 1, x);
    (y - 1, x + 1);
    (y, x - 1);
    (y, x);
    (y, x + 1);
    (y + 1, x - 1);
    (y + 1, x);
    (y + 1, x + 1);
  ]
  |> List.map ~f:(fun (y, x) -> image.(y).(x))

let step algorithm image iteration =
  let image =
    Image.surround image (List.nth_exn algorithm (1 - (iteration % 2)))
  in
  let yb, xb = Image.bounds image in
  let new_image = Image.init (yb + 1) (xb + 1) in
  for y = 1 to yb - 1 do
    for x = 1 to xb - 1 do
      let bitstring = square_bitstring image (y, x) in
      let pixel = List.nth_exn algorithm (int_of_bitstring bitstring) in
      new_image.(y).(x) <- pixel
    done
  done;
  Image.unsurround new_image

let range n = List.init n ~f:Fn.id
