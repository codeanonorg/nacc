include String

type inner = char

type outer = string

let rec explode = function
  | "" -> []
  | _ as s -> s.[0] :: explode (String.sub s 1 (String.length s - 1))

let extract n = function
  | "" when n > 1 -> None
  | "" -> Some ([], "")
  | _ as s when n > String.length s -> None
  | _ as s when n = String.length s -> Some (explode s, "")
  | _ as s when n = 0 -> Some ([], s)
  | _ as s ->
    Some
      (String.sub s 0 n |> explode, String.sub s n (String.length s - 1))

let rec of_list = function [] -> "" | c :: cs -> String.make 1 c ^ of_list cs

let concat = List.fold_left ( ^ ) ""
