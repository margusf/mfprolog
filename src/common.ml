(* Data type for terms. *)

type term =
	| Atom of string
	| Integer of int
	| Var of string
	| Complex of string * term list

(* Rule is head and list of conditions *)
type rule = term * term list

(* Variable substitutions. *)

let empty_subst = fun id -> Var id
let apply_subst subst id = subst id
let unit_subst id new_term =
  function id1 -> if id1 = id then new_term else Var id1

let rec subst_in_term subst = function
  | Atom a -> Atom a
	| Integer i -> Integer i
  | Var v -> apply_subst subst v
  | Complex (func, args) ->
    Complex (func, List.map (subst_in_term subst) args)

let subst_in_terms subst = List.map (subst_in_term subst)

let compose_subst s1 s2 =
  function id -> subst_in_term s2 (apply_subst s1 id)

(* All variables of a term *)

let rec all_term_vars = function
  | Atom _ -> []
	| Integer _ -> []
  | Var v -> [v]
  | Complex (func, args) -> List.flatten (List.map all_term_vars args)

let term_contains_var term var =
  List.mem var (all_term_vars term)

let unique_list =
  let rec iter ret = function
    | [] -> ret
    | h :: t ->
      if List.mem h ret
      then iter ret t
      else iter (h :: ret) t
  in iter []
  
let unique_termlist_vars terms = 
  unique_list (List.flatten (List.map all_term_vars terms))

let get_all_query_results subst goals =
	let output_var var =
		var, subst var
	and bound_var = function
		| var, Var mapping when var = mapping -> false
		| _ -> true in
	let vars = unique_termlist_vars goals in
		let values = List.map output_var vars in
			(* Throw out mappings X -> X which correspond to unbound variables. *)
			let filtered = List.filter bound_var values in
				List.sort (fun (var1, val1) (var2, val2) -> compare var1 var2) filtered

(* Debugging *)

let debug_enabled = ref false

let enable_debug () = debug_enabled := true

let debug s =
  if !debug_enabled
  then print_endline ("DEBUG: " ^ s)
  else ()

(* Exceptions. *)

exception Internal_error of string

exception Prolog_error of string

(* Global variables. *)

(* List of currently valid rules. *)

let rules = ref [Atom "x", [Atom "x"]]  (* Just to help type inference. *)

(* Total failure condition, called by cut and fail. *)

let global_failure = ref (function () -> ())

