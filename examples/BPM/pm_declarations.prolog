/****************************************************************
 *		    		ACTIVITY LIFECYCLE TRANSITIONS				*
 ****************************************************************/
 
% Events.
event(start(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)).
inputEntity(start(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)).
index(start(ACTIVITYINSTANCE,_ACTIVITY,_CASE),ACTIVITYINSTANCE).

event(complete(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)).
inputEntity(complete(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)).
index(complete(ACTIVITYINSTANCE,_ACTIVITY,_CASE),ACTIVITYINSTANCE).

% Simple fluents.
simpleFluent(lifecycleState(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)=started).
outputEntity(lifecycleState(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)=started).
index(lifecycleState(ACTIVITYINSTANCE,_ACTIVITY,_CASE)=started,ACTIVITYINSTANCE).

simpleFluent(lifecycleState(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)=completed).
outputEntity(lifecycleState(_ACTIVITYINSTANCE,_ACTIVITY,_CASE)=completed).
index(lifecycleState(ACTIVITYINSTANCE,_ACTIVITY,_CASE)=completed,ACTIVITYINSTANCE).


/****************************************************************
*				    		CONSTRAINTS							*
 ****************************************************************/

simpleFluent(constraint1(_CASE)=pending).
outputEntity(constraint1(_CASE)=pending).
index(constraint1(CASE)=pending,CASE).

simpleFluent(constraint1(_CASE)=violated).
outputEntity(constraint1(_CASE)=violated).
index(constraint1(CASE)=violated,CASE).

simpleFluent(constraint1(_CASE)=satisfied).
outputEntity(constraint1(_CASE)=satisfied).
index(constraint1(CASE)=satisfied,CASE).

% No statically determined fluents at this time.

/****************************************************************
*				    		GROUNDING							*
 ****************************************************************/

% Grounding of fluents (defines domain of variables).
grounding(lifecycleState(ID,A,C)=started) :- activityInstance(ID), activity(A), case(C), activityInstanceActivity(ID,A), activityInstanceCase(ID,C).
grounding(lifecycleState(ID,A,C)=completed) :- activityInstance(ID), activity(A), case(C), activityInstanceActivity(ID,A), activityInstanceCase(ID,C).
grounding(constraint1(C)=pending) :- case(C).
grounding(constraint1(C)=violated) :- case(C).
grounding(constraint1(C)=satisfied) :- case(C).

%%% declate cyclic fluents
cyclic(constraint1(_)=pending).
cyclic(constraint1(_)=violated).
cyclic(constraint1(_)=satisfied).

/****************************************************************
*			    		CACHE ORDERING							*
 ****************************************************************/

% Ordering of output entities.

cachingOrder(lifecycleState(_,_,_)=started).
cachingOrder(lifecycleState(_,_,_)=completed).
cachingOrder(constraint1(_)=pending).
cachingOrder(constraint1(_)=violated).
cachingOrder(constraint1(_)=satisfied).
