
:- ['../../vopr_RTEC_compiled.prolog'].


% assert SDE every 10 time-points
updateManySDE(Start, End) :-
	Diff is End-Start,
	Diff =< 10,
	!,
	updateSDE(Start, End).	

updateManySDE(Start, End) :-
	Diff is End-Start,
	Diff > 10,
	NewStart is Start+10,
	updateSDE(Start, NewStart),
	updateManySDE(NewStart, End).
