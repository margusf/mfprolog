type term = Atom of string | Var of string

let rec factor n = n * factor (n - 1)
;;

fa