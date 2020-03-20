(** {1 Error report} 
    WIP
*)

type 'a state
(** Type state *)

val state : 'a option -> int -> int -> string -> 'a state
(** Create a new state *)

val result_of_state : 'a state -> ('a, int * int * string) result
(** Get the result *)

val state_value : 'a state -> 'a option
(** Get the parsed value *)

val state_offset : 'a state -> int
(** Get the current position of the parser inside the string *)

val state_line : 'a state -> int
(** Get the current line position of the parser inside the string *)

val state_rest : 'a state -> string
(** Get the remaining characters of the string beeing parsed *)

val report : ('a, int * int * string) result -> unit

(** {1 Defining parsers} *)

type 'a parser
(** Type parser *)

val ( ~~ ) : (string -> 'a state) -> 'a parser
(** Parser constructor *)

val pure : 'a -> 'a parser
(** Pure parser (stop flux consumption, returns results) *)

val many : 'a parser -> 'a list parser
(** Parse zero or more times a given pattern
    @param  p   a parser *)

val some : 'a parser -> 'a list parser
(** Parse one or more times a given pattern
    @param  p   a parser *)

val check : (char -> bool) -> char parser
(** Test a predicate on the first character of the input.
    Resolve to this character if the predicate is verified *)

(** {2 Combinators and Infix operators} *)

val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
(** Apply *)

val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
(** Map *)

val ( *> ) : 'a parser -> 'b parser -> 'b parser
(** Apply to the right *)

val ( <* ) : 'a parser -> 'b parser -> 'a parser
(** Apply to the left *)

val ( <|> ) : 'a parser -> 'a parser -> 'a parser
(** Alternative *)

(** {2 Utils} *)

val do_parse : 'a parser -> string -> ('a, int * int * string) result
(** Run a parser on a string *)

val ( --> ) : string -> 'a parser -> 'a state
(** Feed a parser with a string (from left to right)
    This is verry different from [do_parse] ! No verifications are made on the
    remaining chars. *)

val ( <-- ) : 'a parser -> string -> 'a state
(** Feed a parser with a string (from right to left)
    [p <-- input] is [input --> p]. This function is just for code convenience. *)
