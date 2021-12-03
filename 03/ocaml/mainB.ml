open Core

let range n = List.init n ~f:(fun x -> x)

let bitstring_to_int str = Scanf.sscanf ("0b" ^ str) "%i" (fun x -> x)

let read_numbers filename =
  In_channel.read_lines filename |> List.map ~f:String.to_list

let most_common_digits numbers comparator =
  (* Return a bitstring where the bit at each index is the most common digit in the given list of numbers,
     according to the passed comparator *)
  let table = Hashtbl.create (module Int) in
  let number_length = List.length (List.hd_exn numbers) in
  numbers
  |> List.iter ~f:(fun number ->
         List.iteri
           ~f:(fun index chr ->
             match chr with
             | '0' -> Hashtbl.decr table index
             | '1' -> Hashtbl.incr table index
             | _ -> failwith "unexpected character")
           number);
  List.map (range number_length) ~f:(fun x ->
      if comparator (Hashtbl.find_exn table x) 0 then '1' else '0')

let o2_digit_generator numbers = most_common_digits numbers ( >= )

let co2_digit_generator numbers = most_common_digits numbers ( < )

let get_rating rating_digit_generator numbers =
  let number_length = List.length (List.hd_exn numbers) in
  List.foldi (range number_length) ~init:numbers ~f:(fun index numbers _ ->
      let needed_digits = rating_digit_generator numbers in
      match numbers with
      | [ _ ] -> numbers
      | _ ->
          List.filter
            ~f:(fun number ->
              Char.equal
                (List.nth_exn number index)
                (List.nth_exn needed_digits index))
            numbers)
  |> fun list -> List.nth_exn list 0

let get_o2_rating = get_rating o2_digit_generator

let get_co2_rating = get_rating co2_digit_generator

let () =
  let numbers = read_numbers "../input" in
  [ get_o2_rating numbers; get_co2_rating numbers ]
  |> List.map ~f:String.of_char_list
  |> List.map ~f:bitstring_to_int
  |> List.reduce_exn ~f:( * ) |> string_of_int |> print_endline
