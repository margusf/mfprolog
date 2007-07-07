open OUnit

let suite = "MFProlog" >:::
[Test_parser.suite]

let _ =
  run_test_tt_main suite
