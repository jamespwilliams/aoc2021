open Core

type t = Start | End | Big of string | Small of string [@@deriving show, eq]

let cave_of_string =
  let is_lowercase = String.for_all ~f:Char.is_lowercase in
  function
  | "start" -> Start
  | "end" -> End
  | s -> if is_lowercase s then Small s else Big s
