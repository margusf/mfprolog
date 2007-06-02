(* Data type for terms. *)

type term = Atom of string | Var of string | App of string * term list

(* Variable substitutions. *)

let empty_subst = fun id -> Var id
let apply_subst subst id = subst id

