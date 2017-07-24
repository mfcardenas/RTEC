
:- ['../../pm_description_compiled.prolog'].


% assert SDE every 327240000 time-points
updateManySDE(Start, End) :-
	Diff is End-Start,
	Diff =< 327240000,
	!,
	updateSDE(Start, End).	

updateManySDE(Start, End) :-
	Diff is End-Start,
	Diff > 327240000,
	NewStart is Start+327240000,
	updateSDE(Start, NewStart),
	updateManySDE(NewStart, End).
