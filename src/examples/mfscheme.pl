% Get rid of warnings about eval.
discontiguous([eval/3]).

%
% Helper functions.
%

% Primitive arithmetics
eval_arith(Op, X, Y, Env, Ret) :-
	eval(X, Env, XA),
	eval(Y, Env, YA),
	T =.. [Op, XA, YA],
	Ret is T.

% Primitive comparison operators
eval_compare(Op, X, Y, Env, true) :-
	eval(X, Env, EX),
	eval(Y, Env, EY),
	T =.. [Op, EX, EY],
	T,
	!.
eval_compare(_Op, _X, _Y, _Env, false).

% map Eval Args
eval_list([], _Env, []).
eval_list([H | T], Env, [EH | ET]) :-
	eval(H, Env, EH),
	eval_list(T, Env, ET).

% Extends environment with new bindings
make_bindings([], [], []).
make_bindings([VH | VT], [BH | BT], [(VH, BH) | Others]) :-
	make_bindings(VT, BT, Others).

make_env(Vars, Vals, env(Bindings)) :-
	make_bindings(Vars, Vals, Bindings).
make_env(Bindings, env(Bindings)).

get_from_env(Var, [env(Bindings) | _], Val) :-
	get_binding_from_list(Var, Bindings, Val).
get_from_env(Var, [_ | Others], Val) :-
	get_from_env(Var, Others, Val).

get_binding_from_list(Var, [(Var, Val) | _], Val).
get_binding_from_list(Var, [_ | Others], Val) :-
	get_binding_from_list(Var, Others, Val).

% Separates last element
% split_last([1, 2, 3, 4, 5], [1, 2, 3, 4], 5)
split_last([X], [], X).
split_last([H | T], [H | Split], Last) :-
	split_last(T, Split, Last).

% [(a, b), (c, d), ...] -> [(a, eval(b)), (c, eval(d), ...]
eval_let_args([], _Env, []).
eval_let_args([(Var, Val) | T], Env, [(Var, EVal) | ET]) :-
	eval(Val, Env, EVal),
	eval_let_args(T, Env, ET).

%
% The glorious eval function.
%

% Primitives
eval(X, _Env, X) :- number(X), !.
eval(true, _Env, true) :- !.
eval(false, _Env, false) :- !.

% List support
eval([], _Env, []).
eval([H | T], Env, [EH | ET]) :- eval(H, Env, EH), eval(T, Env, ET).

% Variable access
eval(Var, Env, Val) :- atom(Var), get_from_env(Var, Env, Val).

% Arithmetics
eval(X + Y, Env, Ret) :- eval_arith(+, X, Y, Env, Ret).
eval(X - Y, Env, Ret) :- eval_arith(-, X, Y, Env, Ret).
eval(X * Y, Env, Ret) :- eval_arith(*, X, Y, Env, Ret).
eval(X / Y, Env, Ret) :- eval_arith(/, X, Y, Env, Ret).

eval(X == Y, Env, Ret) :- eval_compare(==, X, Y, Env, Ret).
eval(X \== Y, Env, Ret) :- eval_compare(\==, X, Y, Env, Ret).

% if(if_expr, then_expr, else_expr)
eval(if(If, Then, Else), Env, Ret) :-
	eval(If, Env, EIf),
	!,
	(
		(EIf == true,
	  		eval(Then, Env, Ret));
		eval(Else, Env, Ret)).

% print(expr)
eval(print(X), Env, true) :-
	!,
	eval(X, Env, EX),
	print(EX).
eval(println(X), Env, true) :-
	!,
	eval(X, Env, EX),
	format("~p~N", [EX]).

% sequence operator (stuff1, stuff2)
eval((X, Y), Env, Ret) :-
	!,
	eval(X, Env, _),
	eval(Y, Env, Ret).

% apply(fun, [args])
eval(apply(Fun, Args), Env, Ret) :-
	!,
	eval(Fun, Env, fun(FArgs, FBody)),
	eval_list(Args, Env, EArgs),
	make_env(FArgs, EArgs, NewEnv),
	eval(FBody, [NewEnv | Env], Ret).

% lambda([args], body)
eval(lambda(Args, Body), _Env, fun(Args, Body)) :- list(Args).

% Convenience form: let(var, val, body)
eval(let(Var, Val, Body), Env, Ret) :-
	atom(Var),
	!,
	eval(let((Var, Val), Body), Env, Ret).
% let((var1, val1), (var2, val2), ..., body)
eval(Expr, Env, Ret) :-
	Expr =.. [let | LetArgs],
	!,
	split_last(LetArgs, Args, Body),
	eval_let_args(Args, Env, Bindings),
	make_env(Bindings, NewEnv),
	eval(Body, [NewEnv | Env], Ret).

% Convenience method for writing procedure calls as f(x, y, z, ...).
eval(Funcall, Env, Ret) :-
	compound(Funcall),
	Funcall =.. [Fun | Args],
	eval(apply(Fun, Args), Env, Ret), !.

% Entry point for REPL
eval(Expr) :- eval(Expr, [], Ret), print(Ret).

% let(fact, lambda([n], (println(n), if(n == 1, 1, n * fact(n - 1)))), fact(5))
