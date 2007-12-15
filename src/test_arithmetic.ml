open Common
open Test_helper
open OUnit
 
let tests = add_rules [] [
  "1 is 1.",
  ["1 is 1."];

  "6 is 2 + 2 * 2.",
  ["6 is 2 + 2 * 2."];

  "8 is (2 + 2) * 2.",
  ["8 is (2 + 2) * 2."];

  "X is 2 + 2, Y is X * 100.",
  ["4 is 2 + 2, 400 is 4 * 100."];
]

let suite = "Arithmetic" >:::
 [("test_arithmetic" >:: (fun () -> run_tests tests))]
