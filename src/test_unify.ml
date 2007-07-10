open OUnit
open Common
open Unify
open Test_helper

let pattern1 = parse_term "boo(X, Y, foo(aa, oioi(bb, cc), X))"
and pattern2 = parse_term "boo(A, urra(dd, ss), foo(aa, B, tt))"
let Some subst = unify pattern1 pattern2
let do_assert var val_str =
  let expected_val = parse_term val_str in
    assert_equal ~printer:Print.string_of_term
        ~msg:("Matching failed for variable " ^ var)
        (subst var) expected_val
let test_unify _ =
  do_assert "X" "tt.";
  do_assert "Y" "urra(dd, ss)";
  do_assert "A" "tt.";
  do_assert "B" "oioi(bb, cc)"

let suite = "Unify" >::: [("test_unify" >:: test_unify)]
