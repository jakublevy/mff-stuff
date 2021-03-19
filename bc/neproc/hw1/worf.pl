rika(_, [], []).
rika(muz, [H|T], [H|PT]) :- call(H), rika(muz, T, PT).
rika(zena, X, P) :- rika(zenaP, X, P); rika(zenaN, X, P).
rika(zenaP, [H|T], [H|PT]) :- call(H), rika(zenaN,T,PT).
rika(zenaN, [H|T], PT) :- not(H), rika(zenaP,T,PT).

worf(Dite, R1, R2) :-
	maplist([X]>>member(X, [muz, zena]), [Dite, R1, R2]),
	R1 \= R2,
	rika(R1, [rika(Dite, [Dite=muz], T)], _),
	rika(R2, [Dite=zena, T=[]], _).
