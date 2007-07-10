open Common

(* Helper functions for test programs. *)

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf
and parse_term s =
  let lexbuf = Lexing.from_string s in
    Grammar.term Lexer.token lexbuf

let cons x y = Complex ("cons", [x; y])
and nil = Atom "nil"
