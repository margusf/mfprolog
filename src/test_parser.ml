open OUnit
open Common

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf

let rec try_conjunct = function
  | [] -> ()
  | (str, parsed) :: tail ->
    let con = parse_conjunct str in
      assert_equal ~printer:Print.string_of_term_list
                   ~msg:"Conjunct parsing failed"
                   con parsed;
      try_conjunct tail

let test_parse_terms _ =
  try_conjunct [
    "simpleatom, with_underscore.",
    [Atom "simpleatom"; Atom "with_underscore"];]

let test_parse_lists _ =
  assert_bool "surprise!" true

let test_parse_rules _ =
  assert_bool "surprise!" true

let suite = "Parser" >::: [
  ("test_parse_terms" >:: test_parse_terms);
  ("test_parse_lists" >:: test_parse_lists);
  ("test_parse_rules" >:: test_parse_rules)]