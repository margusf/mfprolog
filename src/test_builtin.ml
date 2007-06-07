open Common
open Print
open Inference
;;
print_endline "\n\nTesting builtin functions";
(*enable_debug ();*)
 
rules := [
  (* Simple inference. *)
  Complex ("test_print", [Var "X"]),
  [Complex ("print", [Var "X"])]];;

let goals = [
  Complex ("test_print",
           [Complex ("stuff",
                     [Atom "aa"; Atom "bb"])]);
  Complex ("test_print",
           [Complex ("cons",
                     [Atom "aa";
                      Complex ("cons",
                               [Atom "bb"; Atom "nil"])])]);
  Complex ("test_print",
           [Complex ("cons",
                     [Atom "aa";
                      Complex ("cons",
                               [Atom "bb"; Atom "cc"])])])]
and test_goal goal =
  print_endline ("\nSolving goal " ^ (string_of_term goal));
  solve [goal] in
  List.iter test_goal goals;;
