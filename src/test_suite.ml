open OUnit

let suite = "MFProlog" >:::
[Test_parser.suite; Test_unify.suite; Test_inference.suite;
 Test_builtin.suite]

let _ =
  run_test_tt_main suite
