open Base
open Parsing

(**
   Wraps [p] with [l] to its left, and [r] to its right.
   Returns only [p] result.*)
let pwrap l r p =
  let* _ = l in
  let* v = p in
  let* _ = r in
  !> v

(**
   Parse a single digit from 0 to 9. *)
let pdigit = anychar_in "0123456789"

(**
   Parse a natural number, into an int. *)
let pnat = many pdigit ||> String.combine ||> int_of_string

(**
   Parse an integer, into an int. *)
let pint = P(fun inp ->
    match parse (pchar '-') inp with
    | None -> parse pnat inp
    | Some(_, rest) -> parse pnat rest |> pmap ((-) 0)
  )

(**
   Trim unwanted input as matched by [s], from both sides of [p]. *)
let ptrim s p =
  let* _ = some (anychar_in s) in
  let* e = p in
  let* _ = some (anychar_in s) in
  !> e

(**
   Parse a real number, into a float. *)
let pfloat =
  let _float_full =
    let* i = many pdigit ||> String.combine in
    let* d = pchar '.' ||> String.make 1 in
    let* f = many pdigit ||> String.combine in
    !> (i ^ d ^ f) ||> float_of_string
  in
  _float_full <|> (pint ||> float_of_int)

(**
   Parse an unary operator [ident] and its parameter [expr], returning (ident,expr). *)
let punop ident expr =
  let* p = ident in
  let* e = expr in
  !> (p,e)

(**
   Parse a binary operation [op] and both operands [exprl] and [exprr], returning (exprl,op,exprr).*)
let pbinop exprl op exprr =
  let* e1 = exprl in
  let* p = op in
  let* e2 = exprr in
  !> (p,e1,e2)

(**
   Parse a statement [s] delimited on its end with [d]. Returns [s]. *)
let pstmt s d =
  let* s = s in
  let* _ = d in
  !> s
