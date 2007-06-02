open List

(* Data type for terms. *)

type term = Atom of string | Var of string | Complex of string * term list

(* Variable substitutions. *)

let empty_subst = fun id -> Var id
let apply_subst subst id = subst id
let unit_subst id new_term =
  function id1 -> if id1 = id then new_term else Var id1

let rec subst_in_term subst = function
  | Atom a -> Atom a
  | Var v -> apply_subst subst v
  | Complex (func, args) -> Complex (func, map (subst_in_term subst) args)


(* Unification *)

let rec all_term_vars = function
  | Atom _ -> []
  | Var v -> [v]
  | Complex (func, args) -> flatten (map all_term_vars args)

let term_contains_var term var =
  mem var (all_term_vars term)

let unify term1 term2 =
  match term1, term2 with
    | Var t1v, Var t2v -> Some (unit_subst t1v term2)
    | Var t1v, _ when not (term_contains_var term2 t1v) -> Some (unit_subst t1v term2)
