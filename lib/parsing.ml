(**************************************************************************)
(*                           _   _    _    ____ ____                      *)
(*                          | \ | |  / \  / ___/ ___|                     *)
(*                          |  \| | / _ \| |  | |                         *)
(*                          | |\  |/ ___ \ |__| |___                      *)
(*                          |_| \_/_/   \_\____\____|                     *)
(*                                                                        *)
(*                                                                        *)
(*                        Copyright (c) 2020 - CodeAnon                   *)
(**************************************************************************)

type 'a state = ('a option * int * string)

(** Type parser *)
type 'a parser = P of (string -> 'a state)

(** Pure parser (stop flux consumption, returns results) *)
let pure x = P (fun input -> (Some x, 0, input))

(** Apply *)
let ( <*> ) (P p1) (P p2) =
  P
    (fun input ->
       match p1 input with
       | (Some f, o', input') -> (
           match p2 input' with
           | (Some x, o'', input'') -> (Some (f x), o'+o'', input'')
           | (None, o'', input'') -> (None, o'+o'', input''))
       | (None, o', input') -> (None, o', input')
    )

(** Map *)
let ( <$> ) f p = pure f <*> p

(** Apply to the right *)
let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2

(** Apply to the left *)
let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

(** Alternative *)
let ( <|> ) (P p1) (P p2) =
  P (fun input -> match p1 input with (None, _, _) -> p2 input | x -> x)

(** Run a parser on a string *)
let do_parse (P p) input =
  match p input with (Some x, _, _) -> Result.ok x | (None, o, inp) -> Result.error (o,inp)

(** Feed a parser with a string (from left to right)
    This is verry different from [do_parse] ! No verifications are made on the
    remaining chars. *)
let ( --> ) inp (P p) = p inp

(** Feed a parser with a string (from right to left)
    [p <-- input] is [input --> p]. This function is just for code convenience. *)
let ( <-- ) (P p) inp = p inp

(** Parse zero or more times a given pattern
    @param  p   a parser *)
let rec many p = P (fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)

(** Parse one or more times a given pattern
    @param  p   a parser *)
let some p = P (fun inp -> List.cons <$> p <*> many p <-- inp)

(** Check a predicate on the first character of the input.
    Resolve to this character if the predicate is verified *)
let check pred =
  P
    (fun input ->
       match input with
       | s when s <> "" && pred s.[0] ->
         (Some s.[0], 1, String.(sub s 1 (length s - 1)))
       | _ -> (None, 0, input))

(** Alias for constructor [P] *)
let ( ~~ ) f = P f
