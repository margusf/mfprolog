open Common

(* Printing of terms *)

(* TODO: pretty-printing of complex terms *)

let rec string_of_term = function
  | Atom a -> a
  | Var v -> v
  | Complex (func, args) ->
    func ^ "(" ^ (String.concat ", " (List.map string_of_term args)) ^ ")"
let string_of_terms terms = String.concat "\n" (List.map string_of_term terms)
