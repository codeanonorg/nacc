module String =
struct
  include String

  let explode s =
    let rec step lc i =
      if i = length s then lc
      else step (lc @ [s.[i]]) (i+1)
    in
    step [] 0

  let combine = List.fold_left (fun acc c -> acc ^ String.make 1 c) ""

  let combine_str = List.fold_left (^) ""

  let substr l r s = String.sub s l ((length s) - 1 - r)
  let rest = substr 1 0
end

open Base

(**
   Single character parser. Parses [c] in front of the input
*)
let pchar c =
  P(function
     | "" -> None
     | x when x.[0] = c -> Some(c, String.rest x)
     | _ -> None)

(**
   Parser that applies [p] zero or more times.
   Returns the result as a list. *)
let rec some (p: 'a parser) =
  P (fun inp -> match parse p inp with
      | None -> Some([], inp)
      | Some(x, rest) -> match parse (some p) rest with
        | None -> Some([x], rest)
        | Some (lx, r) -> Some(x::lx, r))

(**
   Parser that applies [p] one or more times.
   Returns the result as a list (or None) *)
let many (p: 'a parser) =
  let* x = p in
  let* lx = some p in
  P(fun inp -> Some(x::lx, inp))

(**
   Parser that eats a single character unconditionally. *)
let any = P(fun inp -> Some(String.get inp 0, String.rest inp))

(**
   Parser that eats any of the characters present in the list [cl]. *)
let anychar_of =
  function
  | x::rest -> List.fold_left (<|>) (pchar x) (List.map pchar rest)
  | [] -> pzero

(**
   Parser that accepts any char in string [s]. *)
let anychar_in s = String.explode s |> anychar_of


(**
   Parser that tries to succesfully parse all parsers in order of [pl], or fail on any of them failing. *)
let rec sequence =
  function
  | [] -> pone
  | p::pl ->
    let* x = p in
    let* xs = sequence pl in
    P(fun inp -> Some(x::xs, inp))

(**
   Parse a sequence of characters [cl] *)
let sequence_of_chars cl = sequence (List.map pchar cl)

(**
   Parse the string sequence [s] into a list of characters. *)
let sequence_of_string s = sequence_of_chars (String.explode s)

(**
   Parse the string literal [s]. *)
let literal s = sequence_of_string s ||> String.combine
