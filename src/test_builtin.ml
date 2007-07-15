open Common
open Print
open Inference
open Test_helper
open OUnit
 
let ruleset = parse_rules [
  (* print *)
  "test_print(X) :- print(X).";
  (* cut *)
  "stuff(grr).";
  "test_cut(boo).";
  "test_cut(X) :- stuff(X), !.";
  "test_cut(foo)."]

let tests = add_rules ruleset [
  "test_print(some(complex(term))).",
  ["test_print(some(complex(term)))."];
  "test_cut(A).", ["test_cut(boo).";
                   "test_cut(grr)."]]

let suite = "Builtin" >:::
 [("test_builtin" >:: (fun () -> run_tests tests))]
