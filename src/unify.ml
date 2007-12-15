open Common

(* Unification *)

let rec unify term1 term2 =
  match term1, term2 with
    | Atom a1, Atom a2 -> if a1 = a2 then Some empty_subst else None
    | Integer i1, Integer i2 -> if i1 = i2 then Some empty_subst else None
    | Var v1, (Var v2 as t2) -> Some (unit_subst v1 t2)
    | Var v1, t2 when not (term_contains_var t2 v1) -> Some (unit_subst v1 t2)
    | Var v1, _ -> None
    | _, Var v2 -> unify term2 term1
    | Complex (func1, args1), Complex (func2, args2) -> 
      if func1 = func2 then unify_terms args1 args2 else None
    | _, _ -> None
and unify_terms terms1 terms2 =
  match terms1, terms2 with
    | [], [] -> Some empty_subst
    | h1 ::  t1, h2 :: t2 ->
      (match unify h1 h2 with
         | None -> None
         | Some head_subst ->
           let new_tail1 = subst_in_terms head_subst t1
           and new_tail2 = subst_in_terms head_subst t2 in
             (match unify_terms new_tail1 new_tail2 with
                | None -> None
                | Some tail_subst -> Some (compose_subst head_subst tail_subst)))
    | _ -> None
