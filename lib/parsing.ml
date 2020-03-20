module type STREAM = sig
  type 'a t

  val peek : 'a t -> ('a * 'a t) option

  val npeek: int -> 'a t -> ('a list * 'a t) option

  val empty: 'a t
end

module Parser (In : STREAM) = struct
  type ('a, 'r) state = 'r option * int * 'a In.t

  type ('a, 'r) parsefun = 'a In.t -> ('a, 'r) state

  type ('a, 'r) t = P of ('a, 'r) parsefun

  type ('a, 'r) parser = ('a, 'r) t

  let parse (P p) inp = p inp

  (* TODO: Error handling module *)
  let do_parse (P p) inp =
    match p inp with
    | Some x, _, r when r = In.empty -> Result.ok x
    | Some _, o, r -> Result.error (o, r)
    | None, o, r -> Result.error (o, r)

  let pure v = P (fun inp -> (Some v, 0, inp))
  let nothing = P(fun inp -> (None, 0, inp))

  let eat =
    P
      (fun inp ->
         match In.peek inp with
         | Some (c, rest) -> (Some c, 1, rest)
         | _ -> (None, 0, inp))

  let check f =
    P
      (fun inp ->
         match In.peek inp with
         | Some (c, rest) when f c -> (Some c, 1, rest)
         | _ -> (None, 0, inp))

  let ( <*> ) (P p1) (P p2) =
    P
      (fun inp ->
         match p1 inp with
         | Some f, o', inp' -> (
             match p2 inp' with
             | Some x, o'', inp'' -> (Some (f x), o' + o'', inp'')
             | _ -> (None, o', inp') )
         | _ -> (None, 0, inp))

  let ( <$> ) f p = pure f <*> p

  let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2

  let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

  let ( <|> ) (P p1) (P p2) =
    P (fun inp -> match p1 inp with None, _, _ -> p2 inp | x -> x)

  let ( --> ) inp (P p) = p inp

  let ( <-- ) = parse

  let rec many p = P (fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)

  let some p = P (fun inp -> List.cons <$> p <*> many p <-- inp)

  let ( ~~ ) f = P f
end
