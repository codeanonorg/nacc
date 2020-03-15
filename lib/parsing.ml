module type INPUT_TYPE = sig
  type outer

  type inner

  val extract : int -> outer -> (inner list * outer) option

  val join : inner list -> outer

  val concat : outer list -> outer
end

module Parser (In : INPUT_TYPE) = struct
  type 'a state = 'a option * int * In.outer

  type input = In.outer

  type 'a parsefun = input -> 'a state

  type 'a parser = P of 'a parsefun

  type 'a t = 'a parser

  exception ParseException of int * In.outer

  let parse (P p) inp = p inp

  let do_parse (P p) inp =
    match p inp with
    | Some x, _, r when r = In.join [] -> x
    | Some _, o, r -> raise (ParseException (o, r))
    | None, o, r -> raise (ParseException (o, r))

  let pure v = P (fun inp -> (Some v, 0, inp))
  let nothing = P(fun inp -> (None, 0, inp))

  let eat =
    P
      (fun inp ->
         match In.extract 1 inp with
         | Some (c :: _, rest) -> (Some c, 1, rest)
         | _ -> (None, 0, inp))

  let check f =
    P
      (fun inp ->
         match In.extract 1 inp with
         | Some (c :: _, rest) when f c -> (Some c, 1, rest)
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
