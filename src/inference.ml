open Common
open Unify
open Print

(* Rule is head and list of conditions *)
type rule = string * string list

(* List of currently valid rules. *)

let rules = ref []

(* Instantiate rule with new variables. *)

(* TODO: better algorithm instead of global variable *)

let current_var_number = ref 0

let next_var_name () =
  "_G" ^ (string_of_int !current_var_number)

let make_subst_vars =
  List.map (function x -> next_var_name ())

let fresh_rule (rhead, rtail) =
  let all_vars = unique_termlist_vars (rhead :: rtail) in
  let subst_vars = make_subst_vars all_vars in
  let collect_subst subst old_var new_var =
    compose_subst (unit_subst old_var (Var new_var)) subst in
  let new_subst = List.fold_left2 collect_subst empty_subst all_vars subst_vars in
    subst_in_term new_subst rhead, subst_in_terms new_subst rtail

(* Inference engine *)

let rec match_terms goals subst success failure =
  match goals with
    | [] -> success subst failure
    | ghead :: gtail ->
      match_term ghead subst
        (fun subst1 failure1 ->
           match_terms gtail subst1 success failure1)
        failure
and match_term term subst success failure =
  let rec match_rule_list = function
    | [] -> failure ()
    | rhead :: rtail ->
      match_term_single_rule rhead term subst success
        (function () -> match_rule_list rtail)
  in match_rule_list !rules
and match_term_single_rule rule term subst success failure =
  let new_head, new_tail = fresh_rule rule in
    match unify new_head term with
      | None -> failure ()
      | Some new_subst ->
        match_terms (subst_in_terms new_subst new_tail) (* New goals *)
          new_subst success failure

(* Main driver *)

let solve goals =
  match_terms goals empty_subst
    (fun subst fkont ->
       print_endline
         (string_of_terms (subst_in_terms subst goals)))
    (fun () -> ())
