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
extend_env(Env, [], [], Env).
extend_env(Env, [VH | VT], [BH | BT], Ret) :-
	extend_env([bind(VH, BH) | Env], VT, BT, Ret).

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
eval(Var, [bind(Var, Val) | _T], Val) :- atom(Var), !.
eval(Var, [_H | T], Val) :- atom(Var), eval(Var, T, Val), !.

% Arithmetics
eval(X + Y, Env, Ret) :- eval_arith(+, X, Y, Env, Ret).
eval(X - Y, Env, Ret) :- eval_arith(-, X, Y, Env, Ret).
eval(X * Y, Env, Ret) :- eval_arith(*, X, Y, Env, Ret).
eval(X / Y, Env, Ret) :- eval_arith(/, X, Y, Env, Ret).

eval(X == Y, Env, Ret) :- eval_compare(==, X, Y, Env, Ret).
eval(X \== Y, Env, Ret) :- eval_compare(\==, X, Y, Env, Ret).

% if
eval(if(If, Then, Else), Env, Ret) :-
	eval(If, Env, EIf),
	!,
	(
		(EIf == true,
	  		eval(Then, Env, Ret));
		eval(Else, Env, Ret)).

% print
eval(print(X), Env, true) :-
	!,
	eval(X, Env, EX),
	print(EX).
eval(println(X), Env, true) :-
	!,
	eval(X, Env, EX),
	format("~p~N", [EX]).

% sequence operator
eval((X, Y), Env, Ret) :-
	!,
	eval(X, Env, _),
	eval(Y, Env, Ret).

% apply
eval(apply(Fun, Args), Env, Ret) :-
	!,
	eval(Fun, Env, fun(FArgs, FBody)),
	eval_list(Args, Env, EArgs),
	extend_env(Env, FArgs, EArgs, NewEnv),
	eval(FBody, NewEnv, Ret).

% lambda
eval(lambda(Args, Body), _Env, fun(Args, Body)) :- list(Args).

eval(let(Var, Val, Body), Env, Ret) :-
	atom(Var), !,
	eval(Val, Env, EVal), !,
	extend_env(Env, [Var], [EVal], EEnv),
	eval(Body, EEnv, Ret).

% Convenience method for writing procedure calls as f(x, y, z, ...).
eval(Funcall, Env, Ret) :-
	compound(Funcall),
	Funcall =.. [Fun | Args],
	eval(apply(Fun, Args), Env, Ret), !.

% let(fact, lambda([n], (println(n), if(n == 1, 1, n * fact(n - 1)))), fact(5))
