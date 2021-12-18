open Core

type regular_number = { n : int; depth : int }

let range n = List.init n ~f:Fn.id

(* return an array of (n, depth) pairs *)
let parse_number input =
  let rec aux input depth =
    match input with
    | '[' :: rest -> aux rest (depth + 1)
    | ']' :: rest -> aux rest (depth - 1)
    | ',' :: rest -> aux rest depth
    | a :: rest -> { n = Char.get_digit_exn a; depth } :: aux rest depth
    | _ -> []
  in
  aux (String.to_list input) (-1)

let explode number =
  let rec find_explosion number index =
    match number with
    | n1 :: n2 :: rest ->
        if n1.depth = n2.depth && n1.depth >= 4 then Some (index, (n1.n, n2.n))
        else find_explosion (n2 :: rest) (index + 1)
    | _ -> None
  in

  let do_explosion number (explosion_index, (left_n, right_n)) =
    List.filter_mapi number ~f:(fun index { n; depth } ->
        if index = explosion_index - 1 then Some { n = n + left_n; depth }
        else if index = explosion_index then Some { n = 0; depth = depth - 1 }
        else if index = explosion_index + 1 then None
        else if index = explosion_index + 2 then Some { n = n + right_n; depth }
        else Some { n; depth })
  in

  let explosion = find_explosion number 0 in
  Option.map explosion ~f:(do_explosion number)

let split number =
  let rec aux number =
    match number with
    | ({ n; depth } as number) :: rest ->
        if n < 10 then
          let rest, did_split = aux rest in
          (number :: rest, did_split)
        else
          ( { n = n / 2; depth = depth + 1 }
            ::
            { n = (if n % 2 = 1 then 1 else 0) + (n / 2); depth = depth + 1 }
            :: rest,
            true )
    | _ -> ([], false)
  in
  let split_number, did_split = aux number in
  if did_split then Some split_number else None

let reduce start =
  List.fold_until (range 1000) ~init:start
    ~f:(fun number _ ->
      match explode number with
      | Some exploded_number -> Continue exploded_number
      | None -> (
          match split number with
          | Some split_number -> Continue split_number
          | None -> Stop number))
    ~finish:(fun _ -> failwith "reduction limit reached")

let add n1 n2 =
  let increment = List.map ~f:(fun { n; depth } -> { n; depth = depth + 1 }) in
  increment n1 @ increment n2 |> reduce

let magnitude number =
  let rec magnitude_aux : regular_number list -> regular_number list = function
    | n1 :: n2 :: rest ->
        if n1.depth = n2.depth then
          { n = (3 * n1.n) + (2 * n2.n); depth = n1.depth - 1 } :: rest
        else n1 :: magnitude_aux (n2 :: rest)
    | l -> l
  in

  List.fold_until (range 1000) ~init:number
    ~f:(fun number _ ->
      let number = magnitude_aux number in
      match number with [ x ] -> Stop x | l -> Continue l)
    ~finish:(fun _ -> failwith "magnitude iteration limit reached")
  |> fun n -> n.n
