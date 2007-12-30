{
open Grammar
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let number = ['0'-'9']
let alnum = lowercase | uppercase | number
let identifier = alnum | '_'

rule token = parse
	| [' ' '\t' '\n'] { token lexbuf }
	| "is" { LIS } (* This must precede definition for atom. *)
	| lowercase identifier* as a { LATOM a }
	| (uppercase | '_') identifier* as v { LVARIABLE v }
	| number+ as n { LINTEGER (int_of_string n) }
	| '.' { LDOT }
	| '!' { LCUT }
	| ":-" { LIMPLIES }
	| ',' { LCOMMA }
	| ';' { LSEMICOLON }
	| '(' { LOPENPAREN }
	| ')' { LCLOSEPAREN }
	| '[' { LOPENBRACKET }
	| ']' { LCLOSEBRACKET }
	| '|' { LHEADTAIL }
	| '+' { LPLUS }
	| '-' { LMINUS }
	| '*' { LMULTIPLY }
	| '/' { LDIVIDE }
	| '<' { LLESSTHAN }
	| "=<" { LLESSEREQUALS }
	| '>' { LGREATERTHAN }
	| ">=" { LGREATEREQUALS }
	| "=:=" { LARITHEQUALS }
	| "=\\=" { LARITHNOTEQUALS }
	| '=' { LUNIFY }
	| "==" { LEQUALS }
	| "\\==" { LNOTEQUALS }
	| "@<" { LTERMLESSTHAN }
	| "@=<" { LTERMLESSEREQUALS }
	| "@>" { LTERMGREATERTHAN }
	| "@>=" { LTERMGREATEREQUALS }
	| eof { LEOF }
