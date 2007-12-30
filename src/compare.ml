open Common

(* Term comparison. *)

(* Comparison operators. *)

let rec term_compare t1 t2 =
	let complex_compare (f1, args1) (f2, args2) =
		let cmp_arity = compare (List.length args1) (List.length args2) in
			if cmp_arity != 0 then
				cmp_arity
			else
				(let cmp_fn = compare f1 f2 in
					if cmp_fn != 0 then
						cmp_fn
					else
						termlist_compare args1 args2)
	in
		
		match t1, t2 with
			(* TODO: prolog spec says, order by age (older variable is smaller). *)
			| Var v1, Var v2 -> compare v1 v2
			| Var _, _ -> -1
			| _, Var _ -> 1
			| Integer i1, Integer i2 -> compare i1 i2
			| Integer _, _ -> -1
			| _, Integer _ -> 1
			| Atom a1, Atom a2 -> compare a1 a2
			| Atom _, _ -> -1
			| _, Atom _ -> 1
			| Complex (f1, arg1), Complex (f2, arg2) ->
					complex_compare (f1, arg1) (f2, arg2)
and termlist_compare l1 l2 =
	match l1, l2 with
		| [], [] -> 0
		| h1 :: t1, h2 :: t2 ->
			let cmp = term_compare h1 h2 in
				if cmp != 0 then
					cmp
				else
					termlist_compare t1 t2
		| _ -> invalid_arg "termlist_compare -- lists of different lengths"

(* Built-in function for term comparison. *)
let builtin_compare pred [left; right] subst success failure =
	if pred (term_compare left right) then
		success subst failure
	else
		failure ()
