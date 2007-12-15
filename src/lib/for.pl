for(Lower, Lower, _Upper).
for(Var, Lower, Upper) :-
	Lower < Upper,
	NewLower is Lower + 1,
	for(Var, NewLower, Upper).

for_each(_Var, []) :- fail.
for_each(Var, [Head | Tail]) :-
	Var = Head;
	for_each(Var, Tail).

