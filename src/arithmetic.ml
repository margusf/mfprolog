(* Mathematical evaluation. Functions is/2 and =:=/2 *)

open Common

let invalid_arguments () =
	raise (Prolog_error "Invalid arguments to arithmetic evaluation") 

let get_var_value subst v =
	match subst v with
		| Integer i ->  i
		| _ -> invalid_arguments ()

let eval_int_fun int_fun a1 a2 =
	match a1, a2 with
		| Integer i1, Integer i2 -> Integer (int_fun i1 i2)
		| _ -> invalid_arguments ()

let eval_add =
	List.fold_left (eval_int_fun ( + )) (Integer 0)
let eval_mul =
	List.fold_left (eval_int_fun ( * )) (Integer 1)

let binary_operators = [
	"+", ( + );
	"-", ( - );
	"*", ( * );
	"/", ( / )]

let eval_op op left right =
	try
		let op_fun = List.assoc op binary_operators in
			op_fun left right
	with Not_found -> raise (Prolog_error ("Unknown operator: " ^ op))

(* Evaluates arithmetic expression expr. *)
let rec eval_expr subst = function
	| Integer i -> i
	| Var v -> get_var_value subst v
	(* All operations have two operands *)
	| Complex (op, [left; right]) ->
		eval_op op (eval_expr subst left) (eval_expr subst right)
	| _ -> invalid_arguments ()

let builtin_is [left; right] subst success failure =
	let right_value = Integer (eval_expr subst right) in
	(* TODO: this is copypaste from builtin.ml *)
	match Unify.unify left right_value with
		| Some new_subst -> success (compose_subst subst new_subst) failure
		| None -> failure ()

let builtin_comparison operator [left; right] subst success failure =
	let left_val = eval_expr subst left
	and right_val = eval_expr subst right in
		if operator left_val right_val then
			success subst failure
		else
			failure ()