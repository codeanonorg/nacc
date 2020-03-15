open Libnacc.Parsers

type expr =
  | Cst of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
[@@deriving variants, show]

open StringParser

let rec parse_exp inp =
  inp
  --> (ebinop2 add '+' ~~parse_mul ~~parse_exp <|> ebinop2 sub '-' ~~parse_mul ~~parse_exp <|> ~~parse_mul)

and parse_mul inp =
  inp
  --> (ebinop2 mul '*' ~~parse_pow ~~parse_mul <|> ebinop2 div '/' ~~parse_pow ~~parse_mul <|> ~~parse_pow)

and parse_pow inp = inp --> (binop2 pow (literal "**") ~~parse_fac ~~parse_pow <|> ~~parse_fac)

and parse_fac inp = inp --> (eparenthesized '(' ~~parse_exp ')' <|> ~~parse_cst)

and parse_cst inp = inp --> (cst <$> floatingpoint)

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

let rec eval = function
  | Cst v -> v
  | Add (l, r) -> eval l +. eval r
  | Sub (l, r) -> eval l -. eval r
  | Mul (l, r) -> eval l *. eval r
  | Div (l, r) -> eval l /. eval r
  | Pow (l, r) -> Float.pow (eval l) (eval r)

let _ =
  while true do
    print_string "Calc # ";
    flush stdout;
    let input = read_line () in
    try
      do_parse ~~parse_exp input |> eval |> string_of_float |> print_endline
    with
    | ParseException(o, _) -> begin
        print_endline ("Parse error at character " ^ string_of_int o);
        print_endline ("\t"^input);
        print_endline ("\t" ^ String.make o ' ' ^ "^")
      end
    | End_of_file ->
      print_endline "Bye.";
      exit 0
  done
