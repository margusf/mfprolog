{
open Grammar
exception Eof
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let number = ['0'-'9']
let alnum = lowercase | uppercase | number
let identifier = alnum | '_'

rule token = parse
	| [' ' '\t' '\n'] { token lexbuf }
	| lowercase identifier* as a { LATOM a }
	| uppercase identifier* as v { LVARIABLE v }
	| '.' { LDOT }
	| '!' { LCUT }
	| ":-" { LIMPLIES }
	| ',' { LCOMMA }
	| '(' { LOPENPAREN }
	| ')' { LCLOSEPAREN }
	| '[' { LOPENBRACKET }
	| ']' { LCLOSEBRACKET }
	| '|' { LHEADTAIL }
