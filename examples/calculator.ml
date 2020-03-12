open Libnacc.Parsing
open Libnacc.Parsers

type expr =
  | Cst of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
[@@deriving variants, show]

let rec parse_exp inp = inp --> begin
    binop add '+' ~~parse_mul
    <|>
    binop sub '-' ~~parse_mul
    <|>
    ~~parse_mul
  end

and parse_mul inp = inp --> begin
    binop mul '*' ~~parse_pow
    <|>
    binop div '/' ~~parse_pow
    <|>
    ~~parse_pow
  end

and parse_pow inp = inp --> begin
    binop pow '^' ~~parse_fac
    <|>
    ~~parse_fac
  end

and parse_fac inp = inp --> begin
    parenthesized '(' ~~parse_exp ')'
    <|>
    ~~parse_cst
  end

and parse_cst inp = inp --> begin
    cst <$> floatingpoint
  end

(* let _ =
  do_parse ~~parse_exp "1+2*3"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1+2)*3"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "( 1 + 2 ) * ( 5 + 96) "
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1+2)*(5+96)"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1*2)+(5*96)"
  |> Option.get |> show_expr |> print_endline
 *)

let rec eval =
  function
  | Cst v -> v
  | Add(l,r) -> (eval l) +. (eval r)
  | Sub(l,r) -> (eval l) -. (eval r)
  | Mul(l,r) -> (eval l) *. (eval r)
  | Div(l,r) -> (eval l) /. (eval r)
  | Pow(l,r) -> Float.pow (eval l) (eval r)

let _ =
  while true do
    print_string "Calc # ";
    flush stdout;
    let fst (a,_) = a in
    try read_line() --> ~~parse_exp |> Option.get |> fst |> eval |> string_of_float |> print_endline
    with
    | Invalid_argument _ -> print_endline "Syntax error"
    | End_of_file ->
      print_endline "Bye.";
      exit 0
  done
