(**
   Parse data (parse result, rest of input)
*)
type 'a pdata = ('a * string)

(**
   Parse result as option of parse data
*)
type 'a presult = 'a pdata option

(**
   Parser type, holding "eat" functions.
*)
type 'a parser = P of (string -> 'a presult)

(**
   Generic parse exception, used in [pleak] when trying to leak a ['a presult] when it failed.
*)
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

(**
   Pure parser.
   [parse (pterm x) s] always returns [Some(x, s)]. *)
let pterm x = P(fun inp -> Some(x, inp))

(**
   [parse p inp] applies the parser [p] to the input string [inp].
*)
let parse p inp =
  match p with
  | P (p) -> p inp

(**
   Construct a parser from a string transform.
   Alias to the constructor [P] which can be used as argument.
   [parser f] is P(f) *)
let parser f = P(f)

(**
   Parse result map.
   [pmap f (parse p inp)] applies the function f
   to result of [parse p inp] application.
*)
let pmap f: 'a presult -> 'b presult =
  function
  | Some(c, out) -> Some(f c, out)
  | None -> None

(**
   Leaks parse result.
   Raises [ParseException] when trying to leak a failed parse. *)
let pleak: 'a presult -> 'a =
  function
  | Some(c, _) -> c
  | None -> raise (ParseException "Attempted to get failed parse")

(**
   Maps parse output as part of the returned parser.
   [parse (fmap f p) inp] is [parse p inp |> pmap f] *)
let fmap f p =
  P (fun inp -> parse p inp |> pmap f)

(**
   Binding operator.
   [parse (p >>= f) inp] is [parse (f r) out] if [parse p inp] returns [Some (r, out)].
*)
let (>>=) p f =
  P begin
    fun inp -> match parse p inp with
      | None -> None
      | Some(v, out) -> parse (f v) out
  end

(**
   Pure parser operator. Alias of [ppure].
   [!> x] is [P(fun inp -> Some(x, inp))]. *)
let (!>) x = pterm x

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
   Parse result transform operator.
   Maps parse result with [f] as part of the parser itself.
   [p ||> f] is [parse p inp |> pmap f]
*)
let (||>) p f = fmap f p
