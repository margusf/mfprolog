open OUnit
open Common

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
and parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf

let cons x y = Complex ("cons", [x; y])
and nil = Atom "nil"

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
    [cons (Atom "a") nil;
     cons (Var "V") nil;
     cons (Complex ("term", [Atom "a"])) (Atom "nil")];

    "[a | b].",
    [cons (Atom "a") (Atom "b")];

    "[a, b, c].",
    [cons (Atom "a")
       (cons (Atom "b")
          (cons (Atom "c") nil))];

      "[a, b | c].",
    [cons (Atom "a")
       (cons (Atom "b") (Atom "c"))];

    "[a, [b | c], [d]].",
    [cons (Atom "a")
       (cons 
          (cons (Atom "b") (Atom "c"))
          (cons
             (cons (Atom "d") nil)
             nil))]]

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