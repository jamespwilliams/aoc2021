open Core

module T = struct
  type t = Start | End | Big of string | Small of string
  [@@deriving show, eq, ord, sexp_of]

  let of_string : string -> t =
    let is_lowercase = String.for_all ~f:Char.is_lowercase in
    function
    | "start" -> Start
    | "end" -> End
    | s -> if is_lowercase s then Small s else Big s
end

include T
include Comparator.Make (T)
