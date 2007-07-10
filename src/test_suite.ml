open OUnit

let suite = "MFProlog" >:::
[Test_parser.suite; Test_unify.suite]

let _ =
  run_test_tt_main suite
