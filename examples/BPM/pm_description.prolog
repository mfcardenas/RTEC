/****************************************************************
 *		    		ACTIVITY LIFECYCLE TRANSITIONS				*
 ****************************************************************/
 
initiatedAt(lifecycleState(ActivityInstanceId, ActivityConceptName, CaseConceptName)=started, T) :-
	happensAt(start(ActivityInstanceId, ActivityConceptName, CaseConceptName), T).

% the rule below is redundant; the one below that is sufficient
%terminatedAt(lifecycleState(ActivityInstanceId, ActivityConceptName, CaseConceptName)=started, T) :-
%	happensAt(complete(ActivityInstanceId, ActivityConceptName, CaseConceptName), T).

initiatedAt(lifecycleState(ActivityInstanceId, ActivityConceptName, CaseConceptName)=completed, T) :-
	happensAt(complete(ActivityInstanceId, ActivityConceptName, CaseConceptName), T).


/****************************************************************
*				    		CONSTRAINTS							*
 ****************************************************************/

initially(constraint1(_CASE)=pending).
 
terminatedAt(constraint1(CASE)=pending, T):-
	happensAt(start(_,analyze_defect,CASE), T).

% I rearranged the conditions of the rule so that happensAt appears first
initiatedAt(constraint1(CASE)=violated, T2):-
	happensAt(complete(_,register,CASE), T1),
	happensAt(start(_,analyze_defect,CASE), T2),
	% below I changed not to \+ to be compatible with the compiler
	\+ holdsAt(constraint1(CASE)=satisfied, T2),
	T2 >= T1 + 10000.
	
% I rearranged the conditions of the rule so that happensAt appears first
initiatedAt(constraint1(CASE)=satisfied, T2):-
	happensAt(complete(_,register,CASE), T1),
	happensAt(start(_,analyze_defect,CASE), T2),
	% below I changed not to \+ to be compatible with the compiler
	\+ holdsAt(constraint1(CASE)=violated, T2),
	T2 < T1 + 10000.
	
