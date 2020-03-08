open Libnacc.Parsing
open Libnacc.Parsers

type expr =
  | Cst of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
[@@deriving variants, show]


let rec parse_exp inp = inp ==> begin
    binop add '+' parse_mul
    <|>
    binop sub '-' parse_mul
    <|>
    parse_mul
  end

and parse_mul inp = inp ==> begin
    binop mul '*' parse_fac
    <|>
    binop div '/' parse_fac
    <|>
    parse_fac
  end

and parse_fac inp = inp ==> begin
    char '(' *> parse_exp <* char ')'
    <|>
    (cst <$> integer)
  end

let x = do_parse parse_exp "1+2*3"
        |> Option.get |> show_expr |> print_endline
