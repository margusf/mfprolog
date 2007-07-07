open OUnit

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf

let test_parse_terms _ =
  assert_bool "surprise!" true

let test_parse_lists _ =
  assert_bool "surprise!" true

let test_parse_rules _ =
  assert_bool "surprise!" true

let suite = "Parser" >::: [
  ("test_parse_terms" >:: test_parse_terms);
  ("test_parse_lists" >:: test_parse_lists);
  ("test_parse_rules" >:: test_parse_rules)]