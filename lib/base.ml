(* Parsing data (value and rest of input)) *)
type 'a pdata = ('a * string)
(* Parsing result *)
type 'a presult = 'a pdata option
(* Parser constructor *)
type 'a parser = P of (string -> 'a presult)

(* Parsing exception, raised when trying to leak the result of a failed parse *)
exception ParseException of string

(**
   Null parser.
   [parse pszero s] always returns None
*)
let pzero = P (fun _ -> None)

(**
   Empty parser.
   [parse pone s] always returns [Some ([], s)].
*)
let pone = P (fun rest -> Some([], rest))

(* Creates a parser that returns [x] as parsing result without consuming input. *)
let pterm x = P(fun inp -> Some(x, inp))
let (!>) x = pterm x

(**
   [parse p inp] applies the parser [p] to the input string [inp].
*)
let parse p inp =
  match p with
  | P (p) -> p inp

(* Construct a parser from a string transform. *)
let parser f = P(f)

(**
   [pmap f (parse p inp)] applies the function f
   to result of [parse p inp] application.
*)
let pmap f: 'a presult -> 'b presult =
  function
  | Some(c, out) -> Some(f c, out)
  | None -> None

(** Try to get the result of a parser, raises [ParseException] *)
let pleak: 'a presult -> 'a =
  function
  | Some(c, _) -> c
  | None -> raise (ParseException "Attempted to get failed parse")

(** [parse (fmap f p) inp] is [parse p inp |> pmap f] *)
let fmap f p =
  P (fun inp -> parse p inp |> pmap f)

(**
   Binding operator.
   [parse (p >>= f) inp] is [parse (f r) out] if [parse p inp] retunrs [Some (r, out)].
*)
let (>>=) p f =
  P begin
    fun inp -> match parse p inp with
      | None -> None
      | Some(v, out) -> parse (f v) out
  end

(**
   Or operator.
   [p <|> q] accepts all strings accepted by [p] or by [q].
*)
let (<|>) p q =
  P (fun inp -> match parse p inp with
      | None -> parse q inp
      | _ as r -> r)

(**
   Syntax sugar for bindings.
   [let* x = p in f x] is [p >>= (fun x -> f x)] where f returns a parser.
*)
let (let*) x f = x >>= f

(**
   Alias for fmap.
*)
let (||>) p f = fmap f p
