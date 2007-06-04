open Common
open Print
open Inference
;;
print_endline "\n\nTesting inference engine";
  
rules := [
  Complex ("likes", [Atom "mary"; Atom "food"]), [];
  Complex ("likes", [Atom "mary"; Atom "wine"]), [];
  Complex ("likes", [Atom "john"; Atom "mary"]), [];
  Complex ("likes", [Atom "john"; Var "X"]),
  [Complex ("likes", [Atom "mary"; Atom "X"])]
];

let goals = [
  Complex ("likes", [Atom "mary"; Atom "wine"]);
  Complex ("likes", [Atom "mary"; Var "X"]);
  Complex ("likes", [Var "X"; Atom "wine"]);
  Complex ("likes", [Atom "john"; Atom "wine"]);
  Complex ("likes", [Atom "mary"; Atom "booze"]);
  Complex ("likes", [Atom "john"; Var "X"])] 
and test_goal goal =
  print_endline ("Solving goal " ^ (string_of_term goal));
  solve [goal] in
List.iter test_goal goals;;