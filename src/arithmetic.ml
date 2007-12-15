(* Mathematical evaluation. Functions is/2 and =:=/2 *)

open Common

let get_var_value subst v =
	Integer 666

let invalid_arguments () =
	raise (Prolog_error "Invalid arguments to arithmetic evaluation") 

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
	let strip_arg = function
		| Integer x -> x
		| _ -> invalid_arguments () in
	try
		let op_fun = List.assoc op binary_operators in
			Integer (op_fun (strip_arg left) (strip_arg right))
	with Not_found -> raise (Prolog_error ("Unknown operator: " ^ op))

(* Evaluates arithmetic expression expr. *)
let rec eval_expr subst expr =
	let eval_params = List.map (eval_expr subst) in
	match expr with
		| Integer i as n -> n
		| Var v -> get_var_value subst v
		(* All operations have two operands *)
		| Complex (op, [left; right]) ->
			eval_op op (eval_expr subst left) (eval_expr subst right)
		| _ -> invalid_arguments ()

let builtin_is [left; right] subst success failure =
	let right_value = eval_expr subst right in
	(* TODO: this is copypaste from builtin.ml *)
	match Unify.unify left right_value with
		| Some new_subst -> success (compose_subst subst new_subst) failure
		| None -> failure ()
