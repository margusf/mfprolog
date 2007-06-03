open Common
open Unify

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
