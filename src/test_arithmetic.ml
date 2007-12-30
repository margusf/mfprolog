open Common
open Test_helper
open OUnit
 
let tests = add_rules [] [
  "1 is 1.",
  [[]];

  "6 is 2 + 2 * 2.",
  [[]];

  "8 is (2 + 2) * 2.",
  [[]];

  "X is 2 + 2, Y is X * 100.",
  [["X", "4"; "Y", "400"]];
]

let suite = "Arithmetic" >:::
 [("test_arithmetic" >:: (fun () -> run_tests tests))]
