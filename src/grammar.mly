%{
open Common;;

(* Convert list into cons term. *)
let rec make_cons terminator = function
	| [] -> terminator
	| head :: tail -> Complex ("cons", [head; make_cons terminator tail])
%}

%token <string> LATOM
%token <string> LVARIABLE

%token LDOT
%token LIMPLIES
%token LCUT
%token LOPENPAREN
%token LCLOSEPAREN
%token LOPENBRACKET
%token LCLOSEBRACKET
%token LHEADTAIL
%token LCOMMA
%token LEOF

%start rule conjunct term rulelist
%type <Common.rule> rule
%type <Common.term list> conjunct
%type <Common.term> term
%type <Common.rule list> rulelist

%%

conjunct:
	term_list LDOT { $1 }
;

term:
	| LATOM { Atom $1 }
	| LVARIABLE { Var $1 }
	| LATOM LOPENPAREN term_list LCLOSEPAREN { Complex ($1, $3) }
	| LCUT { Complex ("cut", []) }
	/* Different options for lists. */
	| LOPENBRACKET LCLOSEBRACKET { Atom "nil" }
	| LOPENBRACKET term_list LCLOSEBRACKET { make_cons (Atom "nil") $2 }
	| LOPENBRACKET term_list LHEADTAIL term LCLOSEBRACKET { make_cons $4 $2 }
;

term_list:
	| term { [$1] }
	| term LCOMMA term_list { $1 :: $3 }
;

rule:
	| term LDOT { $1, [] }
	| term LIMPLIES term_list LDOT { $1, $3 }
;

rules:
	| rule { [$1] }
	| rules rule { $1 @ [$2] }  /* TODO: do not use append */
;

rulelist:
	| LEOF { [] }
	| rules LEOF { $1 }  
;
