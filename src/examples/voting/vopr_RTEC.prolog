/****************************************************************
 *                                                              *
 * A Voting Protocol (VoPr) in RTEC				*
 * Alexander Artikis						*
 *								*
 * Based on the specification of Jeremy Pitt		 	* 
 *                                                              *
 ****************************************************************/


/*
In this example, institutional power is best expressed by statically determined
fluents. Power is NOT used as a condition in the rules expressing the effects 
of actions because cycles cannot include statically determined fluents. 
Power however is defined to answer queries. 
*/

/****************************************
  AGENT ACTIONS 	                                             
  propose(Agent, Motion)           
  second(Agent, Motion)           
  vote(Agent, Motion, aye)      	
  vote(Agent, Motion, nay)      			
  close_ballot(Agent, Motion)          
  declare(Agent, Motion, carried/not_carried)	
 ****************************************/                          

/********************************
  PROTOCOL FLOW 
  propose(Ag,M), second(Ag,M), vote(V1,M,Vote),...,vote(Vn,M,Vote),
  close_ballot(C,M), declare(C, M, Outcome)

  agents start voting as soon as there is a secondment, ie there is no open_ballot
 ********************************/

:- dynamic maxDuration/3.

/*********************
      status(M)
 *********************/

% deadlines on status:
maxDuration(status(M)=proposed, status(M)=null, 10) :- motion(M).
maxDuration(status(M)=voting, status(M)=voted, 10) :- motion(M).
maxDuration(status(M)=voted, status(M)=null, 10) :- motion(M).

% the condition below expresses initially(status(M)=null).
initiatedAt(status(M)=null, T1, -1, T2) :- T1=<(-1), -1<T2.
% in the 2 rules below we do not have a constraint on the role of the agents
% ie anyone may propose or second a motion
initiatedAt(status(M)=proposed, T1, T, T2) :-
	happensAt(propose(P,M), T), T1=<T, T<T2,
	holdsAt(M, status(M)=null, T).
initiatedAt(status(M)=voting, T1, T, T2) :-
	happensAt(second(S,M), T), T1=<T, T<T2,
	holdsAt(M, status(M)=proposed, T).
initiatedAt(status(M)=voted, T1, T, T2) :-
	happensAt(close_ballot(C,M), T), T1=<T, T<T2,
	holdsAt(role_of(C,chair)=true, T),
	holdsAt(M, status(M)=voting, T).
initiatedAt(status(M)=null, T1, T, T2) :-
	happensAt(declare(C,M,_), T), T1=<T, T<T2,
	holdsAt(role_of(C,chair)=true, T),
	holdsAt(M, status(M)=voted, T).

/*********************
    voted(V,M)=Vote
 *********************/

% a voter may vote several times during status(M)=voting
% only the last vote counts
% retracting a vote is achieved by means of vote=null
% the vote actions of non-voters are ignored

initiatedAt(voted(V,M)=Vote, T1, T, T2) :-
	happensAt(vote(V,M,Vote), T), T1=<T, T<T2,
	holdsAt(status(M)=voting, T),	
	holdsAt(role_of(V,voter)=true, T).
initiatedAt(voted(V,M)=null, T1, T, T2) :-
	happensAt(start(status(M)=null), T), T1=<T, T<T2.

/*****************************
      outcome(M)=Outcome
 *****************************/

initiatedAt(outcome(M)=Outcome, T1, T, T2) :-
	happensAt(declare(C,M,Outcome), T), T1=<T, T<T2,
	holdsAt(status(M)=voted, T),	
	holdsAt(role_of(C,chair)=true, T).
terminatedAt(outcome(M)=O, T1, T, T2) :-
	happensAt(start(status(M)=proposed), T), T1=<T, T<T2.

/*********************
  INSTITUTIONAL POWER
 *********************/

holdsFor(pow(propose(P,M))=true, I) :-
	holdsFor(status(M)=null, I).
holdsFor(pow(second(S,M))=true, I) :-
	holdsFor(status(M)=proposed, I).
% a voter is empowered to vote many times until the ballot is closed;
% only the most recent vote counts
holdsFor(pow(vote(V,M))=true, I) :-
	holdsFor(role_of(V,voter)=true, I1),
	holdsFor(status(M)=voting, I2),
	intersect_all([I1,I2], I).
holdsFor(pow(close_ballot(C,M))=true, I) :-
	holdsFor(role_of(C,chair)=true, I1),
	holdsFor(status(M)=voting, I2),
	intersect_all([I1,I2], I).
% the chair is empowered to declare the outcome of the result either way
holdsFor(pow(declare(C,M))=true, I) :-
	holdsFor(role_of(C,chair)=true, I1),
	holdsFor(status(M)=voted, I2),
	intersect_all([I1,I2], I).

/*************
  PERMISSION
 *************/

% we define permission only for actions that we want to sanction

% auxPerCloseBallot is an auxiliary predicate used in the definition of 
% the permission to close the ballot

% deadline of auxPerCloseBallot 
maxDuration(auxPerCloseBallot(M)=true, auxPerCloseBallot(M)=false, 8) :- motion(M).

initiatedAt(auxPerCloseBallot(M)=true, T1, T, T2) :-
	happensAt(start(status(M)=voting), T), T1=<T, T<T2.
initiatedAt(auxPerCloseBallot(M)=false, T1, T, T2) :-
	happensAt(start(status(M)=proposed), T), T1=<T, T<T2.

% the chair is not permitted to close the ballot too early
initiatedAt(per(close_ballot(C,M))=true, T1, T, T2) :-
        happensAt(end(auxPerCloseBallot(M)=true), T), T1=<T, T<T2,
	holdsAt(status(M)=voting, T),
	holdsAt(role_of(C,chair)=true, T).
initiatedAt(per(close_ballot(C,M))=false, T1, T, T2) :-
	happensAt(start(status(M)=voted), T), T1=<T, T<T2.


/*****************
  OBLIGATION
 *****************/

% obligations are associated only with the declare action

% for efficiency, we do not define an obligation for the not_carried option
% instead, we rely on negation by failure---see the definition of sanction below

initiatedAt(obl(declare(C,M,carried))=true, T1, T, T2) :-
	happensAt(start(status(M)=voted), T), T1=<T, T<T2,
	holdsAt(role_of(C,chair)=true, T),
	findall(V, holdsAt(voted(V,M)=aye, T), AyeList), length(AyeList, AL), 
	findall(V, holdsAt(voted(V,M)=nay, T), NayList), length(NayList, NL),
	% standing rules: simple majority
	AL>=NL.
initiatedAt(obl(declare(C,M,carried))=false, T1, T, T2) :-
	happensAt(start(status(M)=null), T), T1=<T, T<T2.

/**********
  SANCTION 
 **********/

% the sanction could be financial penalty, for example

% the chair is sanctioned if it closes the ballot when forbidden to do so
% ie closing the ballot earlier is sanctioned
initiatedAt(sanctioned(C)=true, T1, T, T2) :-
	happensAt(close_ballot(C,M), T), T1=<T, T<T2,
	\+ holdsAt(per(close_ballot(C,M))=true, T).

% the chair is sanctioned if it does not comply with its obligation to declare the 
% correct outcome of the voting procedure
initiatedAt(sanctioned(C)=true, T1, T, T2) :-
	happensAt(end(status(M)=voted), T), T1=<T, T<T2,
	\+ happensAt(declare(C,M,carried), T),
	holdsAt(obl(declare(C,M,carried))=true, T).
initiatedAt(sanctioned(C)=true, T1, T, T2) :-
	happensAt(end(status(M)=voted), T), T1=<T, T<T2,
	\+ happensAt(declare(C,M,not_carried), T),
	\+ holdsAt(obl(declare(C,M,carried))=true, T).


