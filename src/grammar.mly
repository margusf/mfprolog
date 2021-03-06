%{
open Common;;

let make_op op arg1 arg2 =
	Complex (op, [arg1; arg2])
%}

%token <string> LATOM
%token <string> LVARIABLE
%token <int> LINTEGER

%token LDOT
%token LIMPLIES
%token LCUT
%token LOPENPAREN
%token LCLOSEPAREN
%token LOPENBRACKET
%token LCLOSEBRACKET
%token LHEADTAIL
%token LCOMMA
%token LSEMICOLON
%token LEOF
%token LPLUS
%token LMINUS
%token LMULTIPLY
%token LDIVIDE
%token LIS
%token LLESSTHAN
%token LLESSEREQUALS
%token LGREATERTHAN
%token LGREATEREQUALS
%token LARITHEQUALS
%token LARITHNOTEQUALS
%token LUNIFY
%token LEQUALS
%token LNOTEQUALS
%token LTERMLESSTHAN
%token LTERMLESSEREQUALS
%token LTERMGREATERTHAN
%token LTERMGREATEREQUALS


%nonassoc LIS LLESSTHAN LGREATERTHAN LLESSEREQUALS LLGREATEREQUALS
	LARITHEQUALS LARITHNOTEQUALS
	LTERMLESSTHAN LTERMGREATERTHAN LTERMLESSEREQUALS LTERMLGREATEREQUALS LUNIFY
%right LSEMICOLON
%nonassoc LEQUALS LNOTEQUALS
%left LPLUS LMINUS
%left LMULTIPLY LDIVIDE

%start rule conjunct top_term rulelist
%type <Common.rule> rule
%type <Common.term list> conjunct
%type <Common.term> top_term
%type <Common.rule list> rulelist

%%

conjunct:
	term_list LDOT { $1 }
;

/* for parsing terms in unit tests. */
top_term:
	term LEOF { $1 }
;

term:
	  atom { $1 }
	| LATOM LOPENPAREN term_list LCLOSEPAREN { Complex ($1, $3) }
	| LCUT { Complex ("cut", []) }
	/* Different options for lists. */
	| LOPENBRACKET LCLOSEBRACKET { Utils.nil }
	| LOPENBRACKET term_list LCLOSEBRACKET { Utils.make_cons Utils.nil $2 }
	| LOPENBRACKET term_list LHEADTAIL term LCLOSEBRACKET
		{ Utils.make_cons $4 $2 }
	/* Arithmetic stuff */
	| arithmetic_expr { $1 }
	| logical_expr { $1 }
	| number LIS arithmetic_expr { make_op "is" $1 $3 }
	| variable LIS arithmetic_expr { make_op "is" $1 $3 }
	/* Various prolog predefined operators. */
	| term LSEMICOLON term { make_op ";" $1 $3 } 
	| term LUNIFY term { make_op "=" $1 $3 } 
	| term LEQUALS term { make_op "==" $1 $3 } 
	| term LNOTEQUALS term { make_op "\\==" $1 $3 } 
	| term LTERMLESSTHAN term { make_op "@<" $1 $3 }
	| term LTERMLESSEREQUALS term { make_op "@=<" $1 $3 }
	| term LTERMGREATERTHAN term { make_op "@>" $1 $3 }
	| term LTERMGREATEREQUALS term { make_op "@>=" $1 $3 }
;

atom:
	LATOM { Atom $1 }
;

variable:
	LVARIABLE { Var $1 }
;

number:
	LINTEGER { Integer $1 }
;

term_list:
	  term { [$1] }
	| term LCOMMA term_list { $1 :: $3 }
;

arithmetic_expr:
	  number { $1 }
	| variable { $1 }
	| arithmetic_expr LPLUS arithmetic_expr { make_op "+" $1 $3 }
	| arithmetic_expr LMINUS arithmetic_expr { make_op "-" $1 $3 }
	| arithmetic_expr LMULTIPLY arithmetic_expr { make_op "*" $1 $3 }
	| arithmetic_expr LDIVIDE arithmetic_expr { make_op "/" $1 $3 }
	| LOPENPAREN arithmetic_expr LCLOSEPAREN { $2 }
;

logical_expr:
	  arithmetic_expr LLESSTHAN arithmetic_expr { make_op "<" $1 $3 }
	| arithmetic_expr LLESSEREQUALS arithmetic_expr { make_op "=<" $1 $3 }
	| arithmetic_expr LGREATERTHAN arithmetic_expr { make_op ">" $1 $3 }
	| arithmetic_expr LGREATEREQUALS arithmetic_expr { make_op ">=" $1 $3 }
	| arithmetic_expr LARITHEQUALS arithmetic_expr { make_op "=:=" $1 $3 }
	| arithmetic_expr LARITHNOTEQUALS arithmetic_expr { make_op "=\\=" $1 $3 }
	| LOPENPAREN logical_expr LCLOSEPAREN { $2 }
;

rule:
	  term LDOT { $1, [] }
	| term LIMPLIES term_list LDOT { $1, $3 }
;

rules:
	  rule { [$1] }
	| rules rule { $1 @ [$2] }  /* TODO: do not use append */
;

rulelist:
	  LEOF { [] }
	| rules LEOF { $1 }  
;
