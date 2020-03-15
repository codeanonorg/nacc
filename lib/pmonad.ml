module type INPUT_TYPE =
sig
  type outer
  type inner
  val extract : int -> outer -> (inner list * outer) option
  val join : inner list -> outer
  val concat : outer list -> outer
end

module type INPUT_CONTRIB_TYPE =
sig
  include INPUT_TYPE
  val explode : outer -> inner list
end

module Parser(In: INPUT_TYPE) =
struct
  type 'a state = 'a option * int * (In.outer)
  type input = In.outer
  type 'a parsefun = input -> 'a state
  type 'a parser = P of 'a parsefun
  type 'a t = 'a parser
  exception ParseException of int * In.outer
  let parse (P p) inp = p inp
  let do_parse (P p) inp =
    match p inp with
    | (Some x, _, r) when r = (In.join []) -> x
    | (Some _, o, r) -> raise (ParseException (o, r))
    | (None, o, r) -> raise (ParseException(o, r))
  let pure v = P(fun inp -> (Some v, 0, inp))
  let eat = P(fun inp ->
    match In.extract 1 inp with
    | Some(c::_, rest) -> (Some c, 1, rest)
    | _ -> (None, 0, inp))
  let check f = P(fun inp ->
    match In.extract 1 inp with
    | Some(c::_, rest) when f c -> (Some c, 1, rest)
    | _ -> (None, 0, inp))
  let (<*>) (P p1) (P p2) = P(fun inp ->
    match p1 inp with
    | (Some f, o', inp') -> begin
      match p2 inp' with
      | (Some x, o'', inp'') -> (Some (f x), o'+o'', inp'')
      | _ -> (None, o', inp')
      end
    | _ -> (None, 0, inp))
    let (<$>) f p = pure f <*> p
    let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2
    let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2
    let ( <|> ) (P p1) (P p2) =
      P(fun inp -> match p1 inp with (None, _, _) -> p2 inp | x -> x)
    let (-->) inp (P p) = p inp
    let (<--) = parse
    let rec many p = P(fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)
    let some p = P(fun inp -> List.cons <$> p <*> many p <-- inp)
    let (~~) f = P f
end

module ParserContrib(In:INPUT_CONTRIB_TYPE) =
struct
  include Parser(In)
  let elem e = check ((=) e)
  let one_of el = check (fun e -> List.mem e el)
  let one_in s = one_of (In.explode s)
  let binop cons c v = cons <$> v <*> c *> v
  let ebinop const c v = const <$> v <*> (elem c) *> v
  let parenthesized opar v cpar = opar *> v <* cpar
  let eparenthesized eopar v ecpar = parenthesized (elem eopar) v (elem ecpar)
end

module StringParser =
struct
  module String =
  struct
    include String
    type inner = char
    type outer = string
    let rec explode =
      function
      | "" -> []
      | _ as s -> String.get s 0 :: explode (String.sub s 1 ((String.length s)-1))
    let extract n =
      function
      | "" when n > 1 -> None
      | "" -> Some([], "")
      | _ as s when n > String.length s -> None
      | _ as s when n = String.length s -> Some(explode s, "")
      | _ as s when n = 0 -> Some([], s)
      | _ as s -> Some(String.sub s 0 n |> explode, String.sub s n ((String.length s)-1))
    let rec join =
      function
      | [] -> ""
      | c::cs -> String.make 1 c ^ join cs
    let concat = List.fold_left (^) ""
  end
  include ParserContrib(String)
  let integer =
    let convert l =
      String.join l |> int_of_string
    in
    convert <$> some (one_in "0123456789")
  let flatingpoint =
    let convert l =
      String.join l |> float_of_string
    in
    let plist = one_in "0123456789" in
    let concat a b = List.concat [ a; b ] in
    let ( &> ) p1 p2 = concat <$> p1 <*> p2 in
    let maybe p inp =
      match p <-- inp with
      | (Some x, o, r) -> (Some [x], o, r)
      | (None, o, r) -> (Some [], o, r)
    in
    (convert <$> (many plist &> ~~(maybe (elem '.')) &> some plist)) <|> (float_of_int <$> integer)
end
