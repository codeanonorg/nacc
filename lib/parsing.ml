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

type 'a state = 'a option * int * int * string
(** Type state *)

let state v o l r = (v, o, l, r)

let result_of_state = function
  | Some v, _, _, _ -> Ok v
  | None, o, l, r -> Error (o, l, r)

let state_value (x, _, _, _) = x

let state_offset (_, o, _, _) = o

let state_rest (_, _, _, r) = r

let state_line (_, _, l, _) = l

type 'a parser = P of (string -> 'a state)

let do_parse (P p) input =
  match p input with
  | Some x, _, _, _ -> Result.ok x
  | None, o, _, inp -> Result.error (o, inp)

let ( --> ) inp (P p) = p inp

let ( <-- ) (P p) inp = p inp

let pure x = P (fun input -> (Some x, 0, 0, input))

let ( <*> ) p1 p2 =
  P
    (fun input ->
       match p1 <-- input with
       | Some f, o', _, input' -> (
           match p2 <-- input' with
           | Some x, o'', l, input'' -> (Some (f x), o' + o'', l, input'')
           | None, o'', l, input'' -> (None, o' + o'', l, input'') )
       | None, o', l, input' -> (None, o', l, input'))

let ( <$> ) f p = pure f <*> p

let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2

let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

let ( <|> ) p1 p2 =
  P
    (fun input ->
       match p1 <-- input with
       | None, _, _, _ -> p2 <-- input
       | x -> x)

let rec many p = P (fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)

let some p = P (fun inp -> List.cons <$> p <*> many p <-- inp)

let check pred =
  P
    (fun input ->
       match input with
       | s when s <> "" && pred s.[0] ->
         (Some s.[0], 1, (if s.[0] = '\n' then 1 else 0), String.(sub s 1 (length s - 1)))
       | _ -> (None, 0, 0, input))

let ( ~~ ) f = P f
