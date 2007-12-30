open Common
open Inference
open OUnit

(* Helper functions for parsing stuff programs. *)

let parse_rule s =
  let lexbuf = Lexing.from_string s in
    Grammar.rule Lexer.token lexbuf
let parse_conjunct s =
  let lexbuf = Lexing.from_string s in
    Grammar.conjunct Lexer.token lexbuf
let parse_term s =
  let lexbuf = Lexing.from_string s in
    Grammar.top_term Lexer.token lexbuf

(* For testing various inference stuff. *)

let parse_rules = List.map parse_rule
and add_rules r = List.map (function g, e -> r, g, e)

let get_answer test_rules goals =
  rules := test_rules;
  let results = ref [] in
    match_terms goals empty_subst
      (fun subst fkont ->
         results := !results @ [get_all_query_results subst goals];
         fkont ())
      (fun () -> ());
    !results

let print_list lst =
  let s = String.concat "\n" (List.map string_of_results lst) in
    "\n[" ^ s ^ "]\n"

let parse_result_list_element =
	List.map (function var, value -> var ^ "", parse_term value)

let run_tests testsuite =
  let do_single_test (ruleset, goal_str, result_list) =
		try
	    let goals = parse_conjunct goal_str
	    and expected = List.map parse_result_list_element result_list in
	    let results = get_answer ruleset goals in
	      assert_equal ~msg:("test_inference failed for goal " ^ goal_str)
	                   ~printer:print_list
	                   expected results
		with
			| Parsing.Parse_error ->
					assert_failure ("Parsing failed for \"" ^ goal_str ^ "\"")
  in List.iter do_single_test testsuite
