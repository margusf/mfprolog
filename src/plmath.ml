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

(* Evaluates arithmetic expression expr. *)
let rec eval_expr subst expr =
	let eval_params = List.map (eval_expr subst) in
	match expr with
		| Integer i as n -> n
		| Var v -> get_var_value subst v
		| Complex ("+", op_params) -> eval_add (eval_params op_params)
		| Complex ("*", op_params) -> eval_mul (eval_params op_params)
		| _ -> invalid_arguments ()

let builtin_is [args_left; args_right] subst success failure =
	let right_value = eval_expr subst args_right in
		success subst failure