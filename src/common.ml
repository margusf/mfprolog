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
  | Complex (func, args) -> Complex (func, List.map (subst_in_term subst) args)

let subst_in_terms subst = List.map (subst_in_term subst)

let compose_subst s1 s2 =
  function id -> subst_in_term s2 (apply_subst s1 id)
