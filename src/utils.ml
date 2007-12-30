open Common

(* Utility functions *)

(* Convert list into cons term. *)
let rec make_cons terminator = function
	| [] -> terminator
	| head :: tail -> Complex ("cons", [head; make_cons terminator tail])

let make_list = make_cons (Atom "nil")
