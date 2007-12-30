open Common
open Print

(* Builtin functions for the interpreter. *)

let builtin_cut args subst success failure =
  success subst !global_failure

let builtin_fail args subst success failure =
  !global_failure ()

let builtin_print args subst success failure =
  (match args with
     | [term] -> print_endline (string_of_term term)
     | _ -> raise (Internal_error "Invalid number of arguments"));
  success subst failure

let builtin_consult args subst success failure =
  match args with
    | [Atom filename] ->
    	Loading.consult_file filename;
 		  success subst failure
    | t -> 
      let err = "Invalid file name: " ^ (string_of_term_list t) in
         raise (Prolog_error err)

let builtin_unify [left; right] subst success failure =
	match Unify.unify left right with
		| Some new_subst -> success (compose_subst subst new_subst) failure
		| None -> failure ()

let builtin_funs = [
  "cut", 0, builtin_cut;
  "fail", 0, builtin_fail;
  "print", 1, builtin_print;
  "consult", 1, builtin_consult;
  "is", 2, Arithmetic.builtin_is;
  "=:=", 2, Arithmetic.builtin_comparison ( = );
  "=\\=", 2, Arithmetic.builtin_comparison ( != );
  ">", 2, Arithmetic.builtin_comparison ( > );
  "<", 2, Arithmetic.builtin_comparison ( < );
  "=", 2, builtin_unify;
]

let builtin_matches name arity =
  function fn, farity, _ -> fn = name && farity = arity

let is_builtin_term = function
  | Complex (name, args) -> 
    let arity = List.length(args) in
      List.exists (builtin_matches name arity) builtin_funs
  | _ -> false

let execute_builtin term subst success failure =
  match term with
    | Complex (name, args) ->
    let arity = List.length args in
    let _, _, fn = List.find (builtin_matches name arity) builtin_funs in
      fn args subst success failure
    | _ -> raise (Internal_error "Invalid builtin function call")
