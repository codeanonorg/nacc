open Libnacc.Base
open Libnacc.Parsing
open Libnacc.Utils

type expr =
  | Val of float
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Div of expr*expr
  | Pow of expr*expr

let trimw p = ptrim " \t\n" p
let extract_full: 'a presult -> 'a =
  function
  | Some(x, "") -> x
  | _ -> raise (ParseException "Could not parse entire expression")

let make_value v = Val v
let make_binop p l r =
  match p with
  | '+' -> Add(l,r)
  | '-' -> Sub(l,r)
  | '*' -> Mul(l,r)
  | '/' -> Div(l,r)
  | '^' -> Pow(l,r)
  | _ as x -> raise (ParseException ("Unknown operator " ^ (String.make 1 x)))

let _value = pfloat ||> make_value

let _pow_op = pchar '^' <|> (literal "**" ||> (fun _ -> '^'))
let rec _add inp =
  parse (
    pbinop (parser _sub |> trimw) (pchar '+' |> trimw) (parser _add |> trimw) make_binop <|> parser _sub
  ) inp
and _sub inp = parse (
  pbinop (parser _term |> trimw) (pchar '-' |> trimw) (parser _add |> trimw) make_binop <|> parser _term
) inp
and _term inp = parse (
  pbinop (parser _div |> trimw) (pchar '*' |> trimw) (parser _term |> trimw) make_binop <|> parser _div
) inp
and _div inp = parse (
  pbinop (parser _pow |> trimw) (pchar '/' |> trimw) (parser _term |> trimw) make_binop <|> parser _pow
) inp
and _pow inp = parse (
  pbinop (parser _atom |> trimw) (_pow_op |> trimw) (parser _pow |> trimw) make_binop <|> parser _atom
) inp
and _atom inp = parse (
  pwrap (pchar '(' |> trimw) (pchar ')' |> trimw) (parser _add) <|> _value
) inp

let expr = parser _add

let rec eval =
  function
  | Val v -> v
  | Add(l,r) -> (eval l) +. (eval r)
  | Sub(l,r) -> (eval l) -. (eval r)
  | Mul(l,r) -> (eval l) *. (eval r)
  | Div(l,r) -> (eval l) /. (eval r)
  | Pow(l,r) -> Float.pow (eval l) (eval r)

let _ =
  while true do
    print_string "Calc # ";
    flush stdout;
    try read_line()
        |> parse expr
        |> extract_full
        |> eval
        |> string_of_float
        |> print_endline
    with
    | ParseException s ->
      Printf.printf "Error: %s\n" s
    | End_of_file ->
      print_endline "Bye.";
      exit 0
  done
