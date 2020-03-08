open Parsing

(** Parse a single char *)
let char c = check ((==) c)

(** Parse on of the char of a given list *)
let one_of cl = check (fun c -> List.mem c cl)

let one_in s = check (String.contains s)

(** Parse on of the char of a given list *)
let spaced p =
  let spaces = many (check ((==) ' ')) in
  spaces *> p <* spaces


let integer =
  let convert l = List.(fold_left (^) "" (map (String.make 1) l)) |> int_of_string in
  convert <$> some (one_in "0123456789")

let binop cons c v =
  cons <$> v <*> char c *> v
