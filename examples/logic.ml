open Libnacc.Parsing
open Libnacc.Parsers

type expr =
  | Atom of char
  | Lconj of expr * expr
  | Ldisj of expr * expr
  | Limpl of expr * expr
  | Lnot of expr
[@@deriving variants, show]

let op_impl = spaced (char '-' *> char '>') *> pure limpl

let op_conj = spaced (char '/' *> char '\\') *> pure lconj

let op_disj = spaced (char '\\' *> char '/') *> pure ldisj

let op_not p = lnot <$> spaced (char 'n' *> char 'o' *> char 't') *> p

let parse_atom = atom <$> one_in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let expr =
  let rec impl inp = inp --> chainr ~~disj op_impl
  and disj inp = inp --> chainl op_disj ~~conj
  and conj inp = inp --> chainl op_conj ~~term
  and negt inp = inp --> op_not ~~term
  and term inp = inp --> (~~negt <|> parenthesized '(' ~~impl ')' <|> parse_atom)
  in
  ~~impl

let _ =
  while true do
    print_string "λ ";
    flush stdout;
    try read_line ()
        |> do_parse expr
        |> function Ok e -> show_expr e |> print_endline | Error _ -> print_endline "parse error"
    with End_of_file -> print_endline "Bye."; exit 0
  done