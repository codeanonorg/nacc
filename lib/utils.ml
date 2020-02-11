open Base
open Parsing

let pwrap l r p =
  let* _ = l in
  let* v = p in
  let* _ = r in
  !> v

let pdigit = anychar_in "0123456789"
let pnat = many pdigit ||> String.combine ||> int_of_string
let pint = P(fun inp ->
  match parse (pchar '-') inp with
  | None -> parse pnat inp
  | Some(_, rest) -> parse pnat rest |> pmap ((-) 0)
)

let ptrim s p =
  let* _ = some (anychar_in s) in
  let* e = p in
  let* _ = some (anychar_in s) in
  !> e

let pfloat =
  let _float_full =
    let* i = many pdigit ||> String.combine in
    let* d = pchar '.' ||> String.make 1 in
    let* f = many pdigit ||> String.combine in
    !> (i ^ d ^ f) ||> float_of_string
  in
  _float_full <|> (pint ||> float_of_int)

let punop ident expr make_unop =
  let* p = ident in
  let* e = expr in
  !> (make_unop p e)

let pbinop exprl op exprr make_binop =
  let* e1 = exprl in
  let* p = op in
  let* e2 = exprr in
  !> (make_binop p e1 e2)

let pstmt s d =
  let* s = s in
  let* _ = d in
  !> s
