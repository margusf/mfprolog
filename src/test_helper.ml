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
    Grammar.term Lexer.token lexbuf

let cons x y = Complex ("cons", [x; y])
and nil = Atom "nil"

(* For testing various inference stuff. *)

let parse_rules = List.map parse_rule
and add_rules r = List.map (function g, e -> r, g, e)

let get_answer test_rules goals =
  rules := test_rules;
  let results = ref [] in
    match_terms goals empty_subst
      (fun subst fkont ->
         results := !results @ [subst_in_terms subst goals];
         fkont ())
      (fun () -> ());
    !results

let print_list lst =
  let s = String.concat "\n" (List.map Print.string_of_term_list lst) in
    "\n[" ^ s ^ "]\n"

let run_tests testsuite =
  let do_single_test (ruleset, goal_str, expected_str) =
    let goals = parse_conjunct goal_str
    and expected = List.map parse_conjunct expected_str in
    let results = get_answer ruleset goals in
      assert_equal ~msg:("test_inference failed for goal " ^ goal_str)
                   ~printer:print_list
                   expected results
  in List.iter do_single_test testsuite
