
:- ['../../RTEC.prolog'].
:- ['vopr_RTEC_declarations.prolog'].
:- ['../../timeoutTreatment.prolog'].

maxDuration(auxPerCloseBallot(M)=true, auxPerCloseBallot(M)=false, 8) :- motion(M).
maxDuration(status(M)=proposed, status(M)=null, 10) :- motion(M).
maxDuration(status(M)=voting, status(M)=voted, 10) :- motion(M).
maxDuration(status(M)=voted, status(M)=null, 10) :- motion(M).

initially(status(M)=null).

initiatedAt(status(M)=proposed, T1, T, T2) :-
	happensAtIE(propose(P,M), T), T1=<T, T<T2,
	holdsAtCyclic(P,role_of(P,proposer)=true, T),
	holdsAtCyclic(M, status(M)=null, T).
initiatedAt(status(M)=voting, T1, T, T2) :-
	happensAtIE(second(S,M), T), T1=<T, T<T2,
	holdsAtCyclic(S,role_of(S,seconder)=true, T),
	holdsAtCyclic(M, status(M)=proposed, T).
initiatedAt(status(M)=voted, T1, T, T2) :-
	happensAtIE(close_ballot(C,M), T), T1=<T, T<T2,
	holdsAtCyclic(C,role_of(C,chair)=true, T),
	holdsAtCyclic(M, status(M)=voting, T).
initiatedAt(status(M)=null, T1, T, T2) :-
	happensAtIE(declare(C,M,_), T), T1=<T, T<T2,
	holdsAtCyclic(C,role_of(C,chair)=true, T),
	holdsAtCyclic(M, status(M)=voted, T).

initiatedAt(voted(_131261,_131262)=_131259, T1, T, T2) :-
	happensAtIE(vote(_131261,_131262,_131259), T), T1=<T, T<T2,
	holdsAtCyclic(_131261,role_of(_131261,voter)=true, T),
	holdsAtCyclic(_131262,status(_131262)=voting, T).
initiatedAt(voted(V,M)=null, T1, T, T2) :-
	initiatedAt(status(M)=null, T1, T, T2).

initiatedAt(outcome(_131261)=_131259, T1, T, T2) :-
	happensAtIE(declare(_131269,_131261,_131259),T), T1=<T, T<T2,
	holdsAtProcessedSimpleFluent(_131269,role_of(_131269,chair)=true,T),
	holdsAtProcessedSimpleFluent(_131261,status(_131261)=voted,T).
terminatedAt(outcome(M)=O, T1, T, T2) :-
	initiatedAt(status(M)=proposed, T1, T, T2).

initiatedAt(per(vote(V,M))=true, T1, T, T2) :-
	initiatedAt(status(M)=voting, T1, T, T2),
	holdsAtCyclic(V,role_of(V,voter)=true, T).
initiatedAt(per(vote(V,M))=false, T1, T, T2) :-
	initiatedAt(status(M)=voted, T1, T, T2).
initiatedAt(per(vote(_131263,_131264))=false, T1, T, T2) :-
	happensAtIE(vote(_131263,_131264,_131271),T), T1=<T, T<T2.
initiatedAt(per(vote(V,M))=false, T1, T, T2) :-
	initiatedAt(role_of(V,voter)=false, T1, T, T2).

initiatedAt(auxPerCloseBallot(M)=true, T1, T, T2) :-
	initiatedAt(status(M)=voting, T1, T, T2).
initiatedAt(auxPerCloseBallot(M)=false, T1, T, T2) :-
	initiatedAt(status(M)=proposed, T1, T, T2).

initiatedAt(per(close_ballot(_131263,_131264))=true, T1, T, T2) :-
	initiatedAt(auxPerCloseBallot(M)=false, T1, T, T2),
	holdsAtProcessedSimpleFluent(_131264,status(_131264)=voting,T),
	holdsAtProcessedSimpleFluent(_131263,role_of(_131263,chair)=true,T).
initiatedAt(per(close_ballot(C,M))=false, T1, T, T2) :-
        initiatedAt(status(M)=voted, T1, T, T2).
initiatedAt(per(close_ballot(C,M))=false, T1, T, T2) :-
        initiatedAt(role_of(C,chair)=false, T1, T, T2).

initiatedAt(per(declare(C,M,carried))=true, T1, T, T2) :-
        initiatedAt(status(M)=voted, T1, T, T2),
	holdsAtCyclic(C,role_of(C,chair)=true, T),
	findall(V, (agent(V),holdsAtCyclic(V,voted(V,M)=aye, T)), AyeList), length(AyeList, AL), 
	findall(V, (agent(V),holdsAtCyclic(V,voted(V,M)=nay, T)), NayList), length(NayList, NL),
	% standing rules: simple majority
	AL >= NL.
initiatedAt(per(declare(C,M,not_carried))=true, T1, T, T2) :-
        initiatedAt(status(M)=voted, T1, T, T2),
	holdsAtCyclic(C,role_of(C,chair)=true, T),
	findall(V, (agent(V),holdsAtCyclic(V,voted(V,M)=aye, T)), AyeList), length(AyeList, AL), 
	findall(V, (agent(V),holdsAtCyclic(V,voted(V,M)=nay, T)), NayList), length(NayList, NL),
	% standing rules: simple majority
	AL < NL.
initiatedAt(per(declare(C,M,_Outcome))=false, T1, T, T2) :-
        initiatedAt(status(M)=null, T1, T, T2).
initiatedAt(per(declare(C,M,_Outcome))=false, T1, T, T2) :-
        initiatedAt(role_of(C,chair)=false, T1, T, T2).

initiatedAt(obl(declare(C,M,Outcome))=true, T1, T, T2) :-
        initiatedAt(per(declare(C,M,Outcome))=true, T1, T, T2).
initiatedAt(obl(declare(C,M,Outcome))=false, T1, T, T2) :-
        initiatedAt(per(declare(C,M,Outcome))=false, T1, T, T2).
initiatedAt(obl(declare(_131263,_131264,_131265))=false, T1, T, T2) :-
	happensAtIE(declare(_131263,_131264,_131265),T), T1=<T, T<T2.
/*
initiatedAt(role_of(_131261,voter)=false, T1, T, T2) :-
	happensAtIE(vote(_131261,_131270,_131271),T), T1=<T, T<T2,
	\+holdsAtCyclic(_131261,per(vote(_131261,_131270))=true,T).
*/
initiatedAt(sanctioned(_131261)=true, T1, T, T2) :-
	happensAtIE(close_ballot(_131261,_131270),T), T1=<T, T<T2,
	\+holdsAtProcessedSimpleFluent(_131261,per(close_ballot(_131261,_131270))=true,T).
/*
initiatedAt(role_of(C,chair)=false, T1, T, T2) :-
	initiatedAt(status(M)=null, T1, T, T2),
	holdsAtCyclic(C,obl(declare(C,M,carried))=true, T),
	\+ happensAtIE(declare(C,M,carried), T).
initiatedAt(role_of(C,chair)=false, T1, T, T2) :-
	initiatedAt(status(M)=null, T1, T, T2),
	holdsAtCyclic(C,obl(declare(C,M,not_carried))=true, T),
	\+ happensAtIE(declare(C,M,not_carried), T).
*/
holdsForSDFluent(pow(propose(_131266,_131267))=true,_131249) :-
	holdsForProcessedSimpleFluent(_131266,role_of(_131266,proposer)=true,_131273),
	holdsForProcessedSimpleFluent(_131267,status(_131267)=null,_131285),
	intersect_all([_131273,_131285],_131249).

holdsForSDFluent(pow(second(_131266,_131267))=true,_131249) :-
	holdsForProcessedSimpleFluent(_131266,role_of(_131266,seconder)=true,_131273),
	holdsForProcessedSimpleFluent(_131267,status(_131267)=proposed,_131285),
	intersect_all([_131273,_131285],_131249).

holdsForSDFluent(pow(vote(_131266,_131267))=true,_131249) :-
	holdsForProcessedSimpleFluent(_131266,role_of(_131266,voter)=true,_131273),
	holdsForProcessedSimpleFluent(_131267,status(_131267)=voting,_131285),
	intersect_all([_131273,_131285],_131249).

holdsForSDFluent(pow(close_ballot(_131266,_131267))=true,_131249) :-
	holdsForProcessedSimpleFluent(_131266,role_of(_131266,chair)=true,_131273),
	holdsForProcessedSimpleFluent(_131267,status(_131267)=voting,_131285),
	intersect_all([_131273,_131285],_131249).

holdsForSDFluent(pow(declare(_131266,_131267))=true,_131249) :-
	holdsForProcessedSimpleFluent(_131266,role_of(_131266,chair)=true,_131273),
	holdsForProcessedSimpleFluent(_131267,status(_131267)=voted,_131285),
	intersect_all([_131273,_131285],_131249).

holdsForSDFluent(per(propose(_131266,_131267))=true,_131249) :-
	holdsForProcessedSDFluent(_131266,pow(propose(_131266,_131267))=true,_131249).

holdsForSDFluent(per(second(_131266,_131267))=true,_131249) :-
	holdsForProcessedSDFluent(_131266,pow(second(_131266,_131267))=true,_131249).

/* code for testing maxDurationUE 
simpleFluent(a(_)=true).  outputEntity(a(_)=true).  index(a(M)=true, M).
simpleFluent(a(_)=false). outputEntity(a(_)=false). index(a(M)=false, M).
maxDurationUE(a(M)=true, a(M)=false, 10) :- motion(M).
cachingOrder2(M, a(M)=true) :- motion(M).
initiatedAt(a(M)=true, T1, T, T2) :-
	happensAtIE(propose(P,M), T), T1=<T, T<T2.
initiatedAt(a(M)=false, T1, T, T2) :-
	happensAtIE(second(P,M), T), T1=<T, T<T2.
*/

cachingOrder2(_131248, status(_131248)=null) :- motion(_131248).
cachingOrder2(_131248, status(_131248)=proposed) :- motion(_131248).
cachingOrder2(_131248, status(_131248)=voting) :- motion(_131248).
cachingOrder2(_131248, status(_131248)=voted) :- motion(_131248).
cachingOrder2(_131248, role_of(_131248,_131249)=true) :- agent(_131248),role(_131249).
/*
cachingOrder2(_131248, voted(_131248,_131249)=aye) :- agent(_131248),motion(_131249).
cachingOrder2(_131248, voted(_131248,_131249)=nay) :- agent(_131248),motion(_131249).
cachingOrder2(_131250, per(vote(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, per(declare(_131250,_131251,carried))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, per(declare(_131250,_131251,not_carried))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, obl(declare(_131250,_131251,carried))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, obl(declare(_131250,_131251,not_carried))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131248, outcome(_131248)=carried) :- motion(_131248).
cachingOrder2(_131248, outcome(_131248)=not_carried) :- motion(_131248).
cachingOrder2(_131248, auxPerCloseBallot(_131248)=true) :- motion(_131248).
cachingOrder2(_131250, pow(propose(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, pow(second(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, pow(vote(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, pow(close_ballot(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, pow(declare(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, per(propose(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, per(second(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131250, per(close_ballot(_131250,_131251))=true) :- agent(_131250),motion(_131251).
cachingOrder2(_131248, sanctioned(_131248)=true) :- agent(_131248).
*/