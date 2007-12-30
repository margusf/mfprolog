open Common

(* Utility functions *)

let cons x y = Complex (".", [x; y])
and nil = Complex ("[]", [])

(* Convert list into cons term. *)
let rec make_cons terminator = function
	| [] -> terminator
	| head :: tail -> Complex (".", [head; make_cons terminator tail])

let make_list = make_cons nil
