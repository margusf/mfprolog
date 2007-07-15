open Common
open Inference
open Test_helper
open OUnit

let family_rules =
  parse_rules
    ["father(bart, homer).";
     "father(lisa, homer).";
     "father(maggie, homer).";
     "mother(bart, marge).";
     "mother(lisa, marge).";
     "mother(maggie, marge).";
     "father(homer, abe).";
     "mother(homer, jessica).";
     "father(millhouse, vanhoyt).";
     "parent(Child, Parent) :- father(Child, Parent).";
     "parent(Child, Parent) :- mother(Child, Parent).";
     "grandparent(C, G) :- parent(C, P), parent(P, G).";
     "grandfather(C, G) :- parent(C, P), father(P, G).";
     "couple(F, M) :- father(C, F), mother(C, M)."]
and johnmary_rules =
  parse_rules
    ["likes(mary, food).";
     "likes(mary, wine).";
     "likes(john, mary).";
     "likes(john, X) :- likes(mary, X)."]
and append_rules =
  parse_rules
   ["append([], L, L).";
    "append([HEAD | TAIL], L, [HEAD | A]) :- append(TAIL, L, A)."]

let family_tests = add_rules family_rules [
  "father(bart, X).", ["father(bart, homer)."];
  "parent(bart, X).", ["parent(bart, homer).";
                       "parent(bart, marge)."];
  "grandparent(bart, X).", ["grandparent(bart, abe).";
                            "grandparent(bart, jessica)."];
  "grandfather(bart, X).", ["grandfather(bart, abe)."];
  (* Result is given once for all child. *)
  "couple(homer, marge).", ["couple(homer, marge).";
                            "couple(homer, marge).";
                            "couple(homer, marge)."]]
and johnmary_tests = add_rules johnmary_rules [
  "likes(mary, wine).", ["likes(mary, wine)."];
  "likes(mary, X).", ["likes(mary, food).";
                      "likes(mary, wine)."];
  "likes(X, wine).", ["likes(mary, wine).";
                      "likes(john, wine)."];
  "likes(john, wine).", ["likes(john, wine)."];
  "likes(mary, booze).", [];
  "likes(john, X).", ["likes(john, mary)."; 
                      "likes(john, food).";
                      "likes(john, wine)."];
  "likes(john, X), likes(mary, X).",
  ["likes(john, food), likes(mary, food).";
   "likes(john, wine), likes(mary, wine)."]]
and append_tests = add_rules append_rules [
  "append([a, b], [c, d], X).",
  ["append([a, b], [c, d], [a, b, c, d])."];
  "append(X, [c, d], [a, b, c, d]).",
  ["append([a, b], [c, d], [a, b, c, d])."]]
let all_tests = family_tests @ johnmary_tests @ append_tests

let suite = "Inference" >:::
 [("test_inference" >:: (fun () -> run_tests all_tests))]
