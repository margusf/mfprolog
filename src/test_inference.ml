open Common
open Print
open Inference
;;
print_endline "\n\nTesting inference engine";
(*enable_debug ();*)
 
rules := [
  (* Simple inference. *)
  Complex ("likes", [Atom "mary"; Atom "food"]), [];
  Complex ("likes", [Atom "mary"; Atom "wine"]), [];
  Complex ("likes", [Atom "john"; Atom "mary"]), [];
  Complex ("likes", [Atom "john"; Var "X"]),
  [Complex ("likes", [Atom "mary"; Var "X"])];

  (* List processing *)
  Complex ("append", [Atom "nil"; Var "L"; Var "L"]), [];
  Complex ("append", [Complex ("cons", [Var "HEAD"; Var "TAIL"]);
                      Var "L";
                      Complex ("cons", [Var "HEAD"; Var "A"])]),
  [Complex ("append", [Var "TAIL";
                       Var "L";
                       Var "A"])];
];

let goals = [
  Complex ("likes", [Atom "mary"; Atom "wine"]);
  Complex ("likes", [Atom "mary"; Var "X"]);
  Complex ("likes", [Var "X"; Atom "wine"]);
  Complex ("likes", [Atom "john"; Atom "wine"]);
  Complex ("likes", [Atom "mary"; Atom "booze"]);
  Complex ("likes", [Atom "john"; Var "X"]);
  Complex ("append",
           [Complex ("cons", [Atom "a";
                              Complex ("cons", [Atom "b"; Atom "nil"])]);
            Complex ("cons", [Atom "c";
                              Complex ("cons", [Atom "d"; Atom "nil"])]);
           Var "X"]);
  Complex ("append",
           [Var "X";
            Complex ("cons", [Atom "c";
                              Complex ("cons", [Atom "d"; Atom "nil"])]);
            Complex ("cons", [Atom "a"; 
                              Complex ("cons",
                                       [Atom "b"; 
                                        Complex ("cons",
                                                 [Atom "c"; 
                                                  Complex ("cons",
                                                           [Atom "d";
                                                            Atom "nil"])])])])])
] 
and test_goal goal =
  print_endline ("\nSolving goal " ^ (string_of_term goal));
  solve [goal] in
List.iter test_goal goals;;
