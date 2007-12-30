open Common
open Unify
open Print
open Builtin

(* Instantiate rule with new variables. *)

(* TODO: better algorithm instead of global variable *)

let current_var_number = ref 0

let next_var_name () =
  current_var_number := !current_var_number + 1;
  "_G" ^ (string_of_int !current_var_number)

let make_subst_vars =
  List.map (function x -> next_var_name ())

let fresh_rule (rhead, rtail) =
  let all_vars = unique_termlist_vars (rhead :: rtail) in
  let subst_vars = make_subst_vars all_vars in
  let collect_subst subst old_var new_var =
    compose_subst (unit_subst old_var (Var new_var)) subst in
  let new_subst = List.fold_left2 collect_subst empty_subst all_vars subst_vars in
  let new_head = subst_in_term new_subst rhead
  and new_tail = subst_in_terms new_subst rtail in
    new_head, new_tail

(* Inference engine *)

let rec match_terms goals subst success failure =
  match goals with
    | [] -> success subst failure
    | ghead :: gtail ->
      match_term ghead subst
        (fun subst1 failure1 ->
           match_terms (subst_in_terms subst1 gtail) subst1 success failure1)
        failure
and match_term term subst success failure =
  if is_builtin_term term
  then execute_builtin term subst success failure
  else
    let rec match_rule_list = function
      | [] -> failure ()
      | rhead :: rtail ->
        match_term_single_rule rhead term subst success
          (function () -> match_rule_list rtail)
    in match_rule_list !rules
and match_term_single_rule rule term subst success failure =
  let new_head, new_tail = fresh_rule rule in
    debug ("Fresh rule: " ^ (string_of_rule (new_head, new_tail)));
    match unify new_head term with
      | None -> failure ()
      | Some new_subst ->
        debug ("Unified goal: " ^ (string_of_term (subst_in_term new_subst term)));
        debug ("Unified head: " ^ (string_of_term (subst_in_term new_subst new_head)));
        debug ("New goals: " ^ (string_of_terms ", " (subst_in_terms new_subst new_tail)));
        match_terms (subst_in_terms (compose_subst new_subst subst) new_tail) (* New goals *)
          (compose_subst subst new_subst) success failure

(* Main driver *)

let string_of_results results =
	let res_str = List.map (function var, value -> var ^ " = " ^
                                                 (string_of_term value))
	                       results in
		if results = [] then
			"Yes"
		else
			String.concat "\n" res_str

let solve goals =
	let answer_found = ref false in
  match_terms goals empty_subst
    (fun subst fkont ->
       print_endline
         (string_of_results (get_all_query_results subst goals));
				answer_found := true;
			 print_newline ();
       fkont ())
    (fun () -> if not !answer_found then
			         	 print_endline "No"
							 else
								 ())
