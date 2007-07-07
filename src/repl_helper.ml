let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf
