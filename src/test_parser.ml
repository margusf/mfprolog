open OUnit
open Common
open Test_helper

let try_conjunct =
  let rec iter i t = 
		match t with
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
		"1, asi(22, 33).",
		[Integer 1; Complex ("asi", [Integer 22; Integer 33])];
    "recursive(stuff, inside(other, Stuff)).",
    [Complex ("recursive",
              [Atom "stuff"; 
               Complex ("inside",
                        [Atom "other"; Var "Stuff"])])]]

let test_parse_arithmetic _ =
  try_conjunct [
    "1, 666.",
    [Integer 1; Integer 666];
    "1 + 2, 1 + X, X + Y + Z.",
    [Complex ("+", [Integer 1; Integer 2]);
		 Complex ("+", [Integer 1; Var "X"]);
		 Complex ("+", [Complex ("+", [Var "X"; Var "Y"]); Var "Z"])];
    "1 - 2, 1 - X, X - Y + Z.",
    [Complex ("-", [Integer 1; Integer 2]);
		 Complex ("-", [Integer 1; Var "X"]);
		 Complex ("+", [Complex ("-", [Var "X"; Var "Y"]); Var "Z"])];
    "1 * 2, 1 / X, X * Y / Z.",
    [Complex ("*", [Integer 1; Integer 2]);
		 Complex ("/", [Integer 1; Var "X"]);
		 Complex ("/", [Complex ("*", [Var "X"; Var "Y"]); Var "Z"])];
		"1 * 2 + 3 / (4 + 5).",
		[Complex ("+", [Complex ("*", [Integer 1; Integer 2]);
		                Complex ("/", [Integer 3;
										               Complex ("+", [Integer 4; Integer 5])])])];
		"1 is 2.",
		[Complex ("is", [Integer 1; Integer 2])];
		
		"aa(bee, X = tsee) == 1 + KALA.",
		[Complex ("==", [Complex ("aa", [Atom "bee";
		                                 Complex ("=", [Var "X"; Atom "tsee"])]);
		                 Complex ("+", [Integer 1; Var "KALA"])])]]

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
  let parsed = parse_rule "grandparent(Child, Gramps) :- parent(Child, X), parent(X, Gramps)."
  and model = Complex ("grandparent", [Var "Child"; Var "Gramps"]),
              [Complex ("parent", [Var "Child"; Var "X"]);
               Complex ("parent", [Var "X"; Var "Gramps"])] in
  assert_equal ~printer:Print.string_of_rule
        ~msg:"Rule parsing failed" parsed model

let test_parse_specials _ =
  try_conjunct [
  "!.", [Complex ("cut", [])]]

let suite = "Parser" >::: [
  ("test_parse_terms" >:: test_parse_terms);
  ("test_parse_arithmetic" >:: test_parse_arithmetic);
  ("test_parse_lists" >:: test_parse_lists);
  ("test_parse_rules" >:: test_parse_rules);
  ("test_parse_specials" >:: test_parse_specials)]