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

let () =
  let numbers = read_numbers "../input" in
  [ most_common_digits numbers ( >= ); most_common_digits numbers ( < ) ]
  |> List.map ~f:String.of_char_list
  |> List.map ~f:bitstring_to_int
  |> List.reduce_exn ~f:( * ) |> string_of_int |> print_endline
