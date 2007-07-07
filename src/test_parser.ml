open OUnit
open Common

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf

let try_conjunct =
  let rec iter i = function
    | [] -> ()
    | (str, parsed) :: tail ->
      let con = parse_conjunct str in
        assert_equal ~printer:Print.string_of_term_list
        ~msg:("Conjunct parsing failed for item " ^ (string_of_int i))
        parsed con;
        iter (i + 1) tail in
  iter 1

let test_parse_terms _ =
  try_conjunct [
    "simpleatom, with_underscore.",
    [Atom "simpleatom"; Atom "with_underscore"];
    "X, AA, AA_BB, Aabee, Aa_bee.",
    [Var "X"; Var "AA"; Var "AA_BB"; Var "Aabee"; Var "Aa_bee"];
    "simple_term(a, b, c).",
    [Complex ("simple_term", [Atom "a"; Atom "b"; Atom "c"])];
    "recursive(stuff, inside(other, Stuff)).",
    [Complex ("recursive",
              [Atom "stuff"; 
               Complex ("inside",
                        [Atom "other"; Var "Stuff"])])]]

let test_parse_lists _ =
  try_conjunct [
    "[].",
    [Atom "nil"];
    "[a], [V], [term(a)].",
    [Complex ("cons", [Atom "a"; Atom "nil"]);
     Complex ("cons", [Var "V"; Atom "nil"]);
     Complex ("cons", [Complex ("term", [Atom "a"]);
                       Atom "nil"])];
    "[a | b].",
    [Complex ("cons", [Atom "a";
                       Atom "b"])];
    "[a, b, c].",
    [Complex ("cons", [Atom "a";
                       Complex ("cons",
                                [Atom "b";
                                 Complex ("cons",
                                          [Atom "c"; Atom "nil"])])])];
    "[a, b | c].",
    [Complex ("cons", [Atom "a";
                       Complex ("cons",
                                [Atom "b";
                                 Atom "c"])])]]

let test_parse_rules _ =
  assert_bool "surprise!" true

let test_parse_specials _ =
  try_conjunct [
  "!.", [Complex ("cut", [])]]

let suite = "Parser" >::: [
  ("test_parse_terms" >:: test_parse_terms);
  ("test_parse_lists" >:: test_parse_lists);
  ("test_parse_rules" >:: test_parse_rules);
  ("test_parse_specials" >:: test_parse_specials)]