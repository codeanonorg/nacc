open Parsing

module type INPUT_CONTRIB_TYPE = sig
  include INPUT_TYPE

  val explode : outer -> inner list
end

module ParserContrib (In : INPUT_CONTRIB_TYPE) = struct
  include Parser (In)



  let elem e = check (( = ) e)

  let one_of el = check (fun e -> List.mem e el)

  let one_in s = one_of (In.explode s)

  let rec seq =
    function
    | [] -> pure []
    | p::pl -> List.cons <$> p <*> seq pl

  let literal s = In.join <$> (In.explode s |> List.map elem |> seq)

  let binop2 cons c vl vr = cons <$> vl <*> c *> vr

  let binop cons c v = binop2 cons c v v

  let ebinop2 cons c vl vr = binop2 cons (elem c) vl vr

  let ebinop cons c v = ebinop2 cons c v v

  let parenthesized opar v cpar = opar *> v <* cpar

  let eparenthesized eopar v ecpar = parenthesized (elem eopar) v (elem ecpar)
end

module StringParser = struct
  module String = struct
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

    let rec join = function [] -> "" | c :: cs -> String.make 1 c ^ join cs

    let concat = List.fold_left ( ^ ) ""
  end

  include ParserContrib (String)

  let integer =
    let convert l = String.join l |> int_of_string in
    convert <$> some (one_in "0123456789")

  let floatingpoint =
    let convert l = String.join l |> float_of_string in
    let plist = one_in "0123456789" in
    let concat a b = List.concat [ a; b ] in
    let ( &> ) p1 p2 = concat <$> p1 <*> p2 in
    let maybe p inp =
      match p <-- inp with
      | Some x, o, r -> (Some [ x ], o, r)
      | None, o, r -> (Some [], o, r)
    in
    convert
    <$> (many plist &> ~~(maybe (elem '.')) &> some plist)
    <|> (float_of_int <$> integer)
end
