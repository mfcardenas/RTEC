
/********************************************************
 *							*
 * Voting Protocol declarations				*
 *							*
 * Implemented in RTEC					*
 * Alexander Artikis					*
 *							*
 ********************************************************/


/********************************************************************** DECLARATIONS ******************************************************
 -Declare the entities of the event description: events, simple and statically determined fluents.
 -For each entity state if it is input or output (simple fluents are by definition output entities).
 -For each input/output entity state its index.
 -For input entities/statically determined fluents state whether the intervals will be collected into a list or built from time-points.
 -Declare the groundings of the fluents and output entities/events.
 -Declare the order of caching of output entities.
 ******************************************************************************************************************************************/

event(propose(_,_)).				inputEntity(propose(_,_)).		index(propose(Ag,_), Ag). 
event(second(_,_)).				inputEntity(second(_,_)).		index(second(Ag,_), Ag). 
event(vote(_,_,_)).				inputEntity(vote(_,_,_)).		index(vote(Ag,_,_), Ag). 
event(close_ballot(_,_)).			inputEntity(close_ballot(_,_)).	index(close_ballot(Ag,_), Ag). 
event(declare(_,_,_)).				inputEntity(declare(_,_,_)).		index(declare(Ag,_,_), Ag). 

simpleFluent(role_of(_,_)=false).		outputEntity(role_of(_,_)=false).		index(role_of(Ag,_)=false, Ag).
simpleFluent(role_of(_,_)=true).		outputEntity(role_of(_,_)=true).		index(role_of(Ag,_)=true, Ag).
simpleFluent(status(_)=null).			outputEntity(status(_)=null).			index(status(M)=null, M).
simpleFluent(status(_)=proposed).		outputEntity(status(_)=proposed).		index(status(M)=proposed, M).
simpleFluent(status(_)=voting).		outputEntity(status(_)=voting).		index(status(M)=voting, M).
simpleFluent(status(_)=voted).			outputEntity(status(_)=voted).			index(status(M)=voted, M).
simpleFluent(voted(_,_)=null).			outputEntity(voted(_,_)=null).			index(voted(Ag,_)=null, Ag).
simpleFluent(voted(_,_)=aye).			outputEntity(voted(_,_)=aye).			index(voted(Ag,_)=aye, Ag).
simpleFluent(voted(_,_)=nay).			outputEntity(voted(_,_)=nay).			index(voted(Ag,_)=nay, Ag).
simpleFluent(outcome(_)=carried).		outputEntity(outcome(_)=carried).		index(outcome(M)=carried, M).
simpleFluent(outcome(_)=not_carried).		outputEntity(outcome(_)=not_carried).		index(outcome(M)=not_carried, M).
simpleFluent(auxPerCloseBallot(_)=true).	outputEntity(auxPerCloseBallot(_)=true).	index(auxPerCloseBallot(M)=true, M).
simpleFluent(auxPerCloseBallot(_)=false).	outputEntity(auxPerCloseBallot(_)=false).	index(auxPerCloseBallot(M)=false, M).
simpleFluent(per(close_ballot(_,_))=true).	outputEntity(per(close_ballot(_,_))=true).	index(per(close_ballot(Ag,_))=true, Ag).
simpleFluent(per(close_ballot(_,_))=false).	outputEntity(per(close_ballot(_,_))=false).	index(per(close_ballot(Ag,_))=false, Ag).
simpleFluent(obl(declare(_,_,_))=true).	outputEntity(obl(declare(_,_,_))=true).	index(obl(declare(Ag,_,_))=true, Ag).
simpleFluent(obl(declare(_,_,_))=false).	outputEntity(obl(declare(_,_,_))=false).	index(obl(declare(Ag,_,_))=false, Ag).
simpleFluent(sanctioned(_)=true).		outputEntity(sanctioned(_)=true).		index(sanctioned(Ag)=true, Ag).
simpleFluent(sanctioned(_)=false).		outputEntity(sanctioned(_)=false).		index(sanctioned(Ag)=false, Ag).

sDFluent(pow(propose(_,_))=true).		outputEntity(pow(propose(_,_))=true).		index(pow(propose(Ag,_))=true, Ag).
sDFluent(pow(propose(_,_))=false).		outputEntity(pow(propose(_,_))=false).		index(pow(propose(Ag,_))=false, Ag).
sDFluent(pow(second(_,_))=true).		outputEntity(pow(second(_,_))=true).		index(pow(second(Ag,_))=true, Ag).
sDFluent(pow(second(_,_))=false).		outputEntity(pow(second(_,_))=false).		index(pow(second(Ag,_))=false, Ag).
sDFluent(pow(vote(_,_))=true).			outputEntity(pow(vote(_,_))=true).		index(pow(vote(Ag,_))=true, Ag).
sDFluent(pow(vote(_,_))=false).		outputEntity(pow(vote(_,_))=false).		index(pow(vote(Ag,_))=false, Ag).
sDFluent(pow(close_ballot(_,_))=true).		outputEntity(pow(close_ballot(_,_))=true).	index(pow(close_ballot(Ag,_))=true, Ag).
sDFluent(pow(close_ballot(_,_))=false).	outputEntity(pow(close_ballot(_,_))=false).	index(pow(close_ballot(Ag,_))=false, Ag).
sDFluent(pow(declare(_,_))=true).		outputEntity(pow(declare(_,_))=true).		index(pow(declare(Ag,_))=true, Ag).
sDFluent(pow(declare(_,_))=false).		outputEntity(pow(declare(_,_))=false).		index(pow(declare(Ag,_))=false, Ag).

/*
SWI Prolog

collectIntervals2(_, _) :- fail.
buildFromPoints2(_, _) :- fail.
sDFluent(_) :- fail.
*/

cyclic(status(_)=null).
cyclic(status(_)=proposed).
cyclic(status(_)=voting).
cyclic(status(_)=voted).

% for input entities/statically determined fluents state whether 
% the intervals will be collected into a list or built from given time-points


% define the groundings of the fluents and output entities/events

grounding(role_of(Ag,R)=true)			:- agent(Ag), role(R).
grounding(status(M)=null)			:- motion(M).
grounding(status(M)=proposed)			:- motion(M).
grounding(status(M)=voting)			:- motion(M).
grounding(status(M)=voted)			:- motion(M).
grounding(voted(Ag,M)=null)			:- agent(Ag), motion(M).
grounding(voted(Ag,M)=aye)			:- agent(Ag), motion(M).
grounding(voted(Ag,M)=nay)			:- agent(Ag), motion(M).
grounding(outcome(M)=carried)			:- motion(M).
grounding(outcome(M)=not_carried)		:- motion(M).
grounding(pow(propose(Ag,M))=true)		:- agent(Ag), motion(M).
grounding(pow(second(Ag,M))=true)		:- agent(Ag), motion(M).
grounding(pow(vote(Ag,M))=true)		:- agent(Ag), motion(M).
grounding(pow(close_ballot(Ag,M))=true)	:- agent(Ag), motion(M).
grounding(pow(declare(Ag,M))=true)		:- agent(Ag), motion(M).
grounding(auxPerCloseBallot(M)=true)		:- motion(M).
grounding(per(close_ballot(Ag,M))=true)	:- agent(Ag), motion(M).
grounding(obl(declare(Ag,M,carried))=true)	:- agent(Ag), motion(M).
grounding(sanctioned(Ag)=true)			:- agent(Ag).


% cachingOrder should be defined for all output entities

cachingOrder(role_of(_,_)=true). 
% =================== cycle ================== % 
cachingOrder(status(_)=null). 
cachingOrder(status(_)=proposed).
cachingOrder(status(_)=voting).
cachingOrder(status(_)=voted).
% =================== cycle ================== % 
cachingOrder(voted(_,_)=aye). 			% depends on role_of and status(M) 
cachingOrder(voted(_,_)=nay).			% depends on role_of and status(M); we do no cache voted(V,M)=null
cachingOrder(outcome(_)=carried).		% depends on role_of and status(M)
cachingOrder(outcome(_)=not_carried).		% depends on role_of and status(M)
cachingOrder(auxPerCloseBallot(_)=true).	% depends on status
cachingOrder(pow(propose(_,_))=true).		% depends on role_of and status(M)
cachingOrder(pow(second(_,_))=true).		% depends on role_of and status(M)
cachingOrder(pow(vote(_,_))=true).		% depends on role_of and status(M)
cachingOrder(pow(close_ballot(_,_))=true).	% depends on role_of and status(M)
cachingOrder(pow(declare(_,_))=true).		% depends on role_of and status(M) 
cachingOrder(per(close_ballot(_,_))=true).	% depends on status, role_of and auxPerCloseBallot
cachingOrder(obl(declare(_,_,_))=true).	% depends on status(M), role_of and voted(V,M)
cachingOrder(sanctioned(_)=true).		% depends on per(close_ballot, status(M) and obl(declare
