open Common

(* Printing of terms *)

(* Checks whether this cdr indicates proper list (as opposed to pair). *)
let is_list_cdr = function 
  | Atom "nil" -> true
  | Complex ("cons", _) -> true
  | _ -> false

let rec string_of_term = function
  | Atom a -> a
  | Var v -> "{" ^ v ^ "}"
  (* Some special cases. *)
  | Complex ("cut", []) -> "!"
  | Complex ("cons", [car; cdr]) -> 
    "[" ^ (string_of_list car cdr) ^ "]"
  (* Generic printing of complex term. *)
  | Complex (func, args) ->
    func ^ "(" ^ (String.concat ", " (List.map string_of_term args)) ^ ")"
and string_of_list car cdr =
  let car_str = string_of_term car in
    match cdr with
      | Atom "nil" -> car_str
      | Complex ("cons", [xcar; xcdr]) ->
        let cdr_str =
          if is_list_cdr xcdr
          then string_of_list xcar xcdr
          else string_of_term cdr in
          car_str ^ ", " ^ cdr_str
      | _ -> car_str ^ " | " ^ (string_of_term cdr)

let string_of_terms separator terms =
  String.concat separator (List.map string_of_term terms)

let string_of_term_list = string_of_terms ", "

let string_of_rule (rulehead, ruletail) =
  (string_of_term rulehead) ^
  if ruletail = []
  then ""
  else " :- " ^ (string_of_terms ", " ruletail)
