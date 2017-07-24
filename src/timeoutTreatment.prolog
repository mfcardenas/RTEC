
% maxDuration(F=V, F=NewV, Duration) states that F=V is subject to deadlines
% More precisely, F=V expires after Duration and then F=NewV is initiated
% attempt(F=V) states an attempt to expire F=V  

maxDuration(F=V, F=NewV, Duration) :-
	maxDurationUE(F=V, F=NewV, Duration).	

% the rule below deals with the case where the initiation of a fluent-value pair 
% that is subject to deadlines takes place within the working memory
initiatedAt(F=NewV, T1, T, T2) :-
	maxDuration(F=V, F=NewV, Duration),
	EarlierT1 is T1-Duration, EarlierT2 is T2-Duration, initTime(InitTime), EarlierT2>InitTime,
	initiatedAt(F=V, EarlierT1, EarlierT, EarlierT2), EarlierT1=<EarlierT, EarlierT<EarlierT2,
	% it proved insufficient to store the above starting point	
	T is EarlierT+Duration,
	EarlierTPlus1 is T-Duration+1,
	indexOf(Index, F=V), 
	\+ brokenOrReInitiated(Index, F=V, EarlierTPlus1, T).

% the rule below deals with the case where the initiation of a fluent-value pair 
% that is subject to deadlines takes place before the working memory
initiatedAt(F=NewV, T1, T, T2) :-
	maxDuration(F=V, F=NewV, Duration),
	indexOf(Index, F=V),
	happensAtProcessed(Index, attempt(F=V), T), T1=<T, T<T2, 
	% attempt(F=V) was caused by events taking place before or on Qi-WM 
	EarlyT is T-Duration, initTime(InitTime), EarlyT=<InitTime,
	EarlierTPlus1 is T-Duration+1,
	\+ brokenOrReInitiated(Index, F=V, EarlierTPlus1, T).

% fluent duration cannot be extended
brokenOrReInitiated(Index, F=V, T1, T2) :-
	brokenOnce(Index, F=V, T1, _, T2), !.
% fluent duration may be extended
brokenOrReInitiated(_Index, F=V, T1, T2) :-
	maxDurationUE(F=V, _, _),
	startingPoints(Index, F=V, SPoints),
	member(SPoint, SPoints), 
	prevTimePoint(SPoint, T), 
	T1=<T, !, T<T2.
brokenOrReInitiated(_Index, F=V, T1, T2) :-
	maxDurationUE(F=V, _, _),
	initiatedAt(F=V, T1, _, T2), !.


