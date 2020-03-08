
type 'a parser = string -> ('a * string) option

(** Pure parser (stop flux consumption, returns results) *)
let pure x : 'a parser = fun input -> Some (x, input)

(** Map *)
let (<$>) f p = fun input ->
  match p input with
  | None -> None
  | Some (x, next) -> Some (f x, next)

(** Apply *)
let (<*>) p1 p2 = fun input ->
  match p1 input with
  | None -> None
  | Some (f, input') ->
    match p2 input' with
    | Some (x, input'')  -> Some (f x, input'')
    | None -> None

(** Apply to the right *)
let ( *> ) p1 p2 =
  (fun _ y -> y) <$> p1 <*> p2

(** Apply to the left *)
let ( <* ) p1 p2 =
  (fun x _ -> x) <$> p1 <*> p2

(** Alternative *)
let (<|>) p1 p2 = fun input ->
  match p1 input with
  | None -> p2 input
  | x -> x

(** Run a parser on a string *)
let do_parse p input =
  match p input with
  | Some (x, "") -> Some x
  | _ -> None

let (==>) inp p = p inp

(** Parse zero or more *)
let rec many (p:'a parser) : 'a list parser = fun inp ->
  (* (List.cons <$> p <*> many p) <|> (pure []) *)
  match p inp with
  | None -> Some ([], inp)
  | Some (x, next) ->
    match many p next with
    | None -> None
    | Some (l, next) -> Some (x::l, next)

(** Parse one or more *)
let some p = fun inp ->
  (* (List.cons <$> p) <*> many p *)
  match p inp with
  | None -> None
  | Some (x, next) ->
    match many p next with
    | None -> None
    | Some (l, next) -> Some (x::l, next)

(** Check a predicate on the first character of the input.
    Resolve to this character if the predicate is verified *)
let check pred = fun input ->
  match input with
  | s when s <> "" && pred s.[0] ->
    Some (s.[0], String.(sub s 1 (length s - 1)))
  | _ -> None
