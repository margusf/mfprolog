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

let builtin_tests = add_rules ruleset [
  "test_print(some(complex(term))).", [[]];
  "test_cut(A).", [["A", "boo"]; ["A", "grr"]]]
	
let compare_tests = add_rules [] [
	(* Variable instantiations. *)
	"1 == 1.", [[]];
	"X = 1, X == 1.", [["X", "1"]];
	"X = 1, X == Y.", [];
	"X = 1, Y = 1, X == Y.", [["X", "1"; "Y", "1"]];
	"X = 1, Y = 2, X == Y.", [];
	"X == Y.", [];

	(* Some sanity checks. *)
	"1 == 1.", [[]];
	"1 == 2.", [];
	"1 \\== 1.", [];
	"1 \\== 2.", [[]];
	"1 @> 2.", [];
	"2 @> 1.", [[]];
	"2 @< 2.", [];
	"1 @< 2.", [[]];
	"1 @>= 2.", [];
	"2 @>= 2.", [[]];
	"3 @>= 2.", [[]];
	"1 @=< 2.", [[]];
	"2 @=< 2.", [[]];
	"3 @=< 2.", [];
		
	(* Check the comparison of various term types. *)
	"B @> A.", [[]];
	"1 @> Y.", [[]];
	"x @> Y.", [[]];
	"x(y, z) @> Y.", [[]];
	"2 @> 1.", [[]];
	"x @> 1.", [[]];
	"x(y, z) @> 1.", [[]];
	"x(y, z) @> x.", [[]];
	"a(a, a) @> a(b).", [[]];
	"b(a, a) @> a(b, b).", [[]];
	"a(b, c) @> a(b, b).", [[]];
]

let suite = "Builtin" >:::
 ["test_builtin" >:: (fun () -> run_tests builtin_tests);
  "test_compare_term" >:: (fun () -> run_tests compare_tests)]
