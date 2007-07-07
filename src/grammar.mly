%{
open Common;;
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

%start rule conjunct
%type <Common.rule> rule
%type <Common.term list> conjunct

%%

conjunct:
	term_list LDOT { $1 }
;

term:
	| LATOM { Atom $1 }
	| LVARIABLE { Var $1 }
	| LATOM LOPENPAREN term_list LCLOSEPAREN { Complex ($1, $3) }
	| LCUT { Complex ("cut", []) }
	| LOPENBRACKET term LHEADTAIL term LCLOSEBRACKET { Complex ("cons", [$2;  $4]) }
;

term_list:
	| term { [$1] }
	| term LCOMMA term_list { $1 :: $3 }
;

rule:
	| term LDOT { $1, [] }
	| term LIMPLIES term_list LDOT { $1, $3 }
;
