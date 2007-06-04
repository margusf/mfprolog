open Common
open Print
open Unify

exception Assert_failed of string

let assert_var subst var value =
  let subst_res = subst var in
    print_endline (var ^ " -> " ^ (string_of_term subst_res));
    if subst_res <> value
    then raise (Assert_failed ("Check failed for variable " ^ var))
    else ()
;;

let pattern1 =
  Complex ("boo",
           [Var "X";
            Var "Y";
            Complex ("foo",
                     [Atom "aa";
                      Complex ("oioi",
                               [Atom "bb"; Atom "cc"]);
                      Var "X"])])
and pattern2 =
  Complex ("boo",
           [Var "A";
            Complex ("urra",
                     [Atom "dd";
                      Atom "ss"]);
            Complex ("foo",
                     [Atom "aa";
                      Var "B";
                      Atom "tt"])])
in

print_endline "Testing inference engine";

print_endline ("pattern1 = " ^ (string_of_term pattern1));
print_endline ("pattern2 = " ^ (string_of_term pattern2));

let Some subst =  unify pattern1 pattern2 in
let do_assert = assert_var subst in
do_assert "X" (Atom "tt");
do_assert "Y" (Complex ("urra", [Atom "dd"; Atom "ss"]));
do_assert "A" (Atom "tt");
do_assert "B" (Complex ("oioi", [Atom "bb"; Atom "cc"]))
;;