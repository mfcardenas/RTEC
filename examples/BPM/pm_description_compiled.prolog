
:- ['../../src/RTEC.prolog'].
:- ['pm_declarations.prolog'].
:- ['../../src/timeoutTreatment.prolog'].

initially(constraint1(_131570)=pending).

initiatedAt(lifecycleState(_131264,_131265,_131266)=started, _131274, _131249, _131276) :-
     happensAtIE(start(_131264,_131265,_131266),_131249),
     _131274=<_131249,
     _131249<_131276.

initiatedAt(lifecycleState(_131264,_131265,_131266)=completed, _131274, _131249, _131276) :-
     happensAtIE(complete(_131264,_131265,_131266),_131249),
     _131274=<_131249,
     _131249<_131276.

initiatedAt(constraint1(_131264)=violated, _131307, _131249, _131309) :-
     happensAtIE(complete(_131272,register,_131264),_131270),_131307=<_131270,_131270<_131309,
     happensAtIE(start(_131282,analyze_defect,_131264),_131249),_131307=<_131249,_131249<_131309,
     \+holdsAtCyclic(_131264,constraint1(_131264)=satisfied,_131249),
     _131249>=_131270+10000.

initiatedAt(constraint1(_131264)=satisfied, _131307, _131249, _131309) :-
     happensAtIE(complete(_131272,register,_131264),_131270),_131307=<_131270,_131270<_131309,
     happensAtIE(start(_131282,analyze_defect,_131264),_131249),_131307=<_131249,_131249<_131309,
     \+holdsAtCyclic(_131264,constraint1(_131264)=violated,_131249),
     _131249<_131270+10000.

terminatedAt(constraint1(_131264)=pending, _131275, _131249, _131277) :-
     happensAtIE(start(_131269,analyze_defect,_131264),_131249),
     _131275=<_131249,
     _131249<_131277.

cachingOrder2(_131248, lifecycleState(_131248,_131249,_131250)=started) :-
     activityInstance(_131248),activity(_131249),case(_131250),activityInstanceActivity(_131248,_131249),activityInstanceCase(_131248,_131250).

cachingOrder2(_131248, lifecycleState(_131248,_131249,_131250)=completed) :-
     activityInstance(_131248),activity(_131249),case(_131250),activityInstanceActivity(_131248,_131249),activityInstanceCase(_131248,_131250).

cachingOrder2(_131248, constraint1(_131248)=pending) :-
     case(_131248).

cachingOrder2(_131248, constraint1(_131248)=violated) :-
     case(_131248).

cachingOrder2(_131248, constraint1(_131248)=satisfied) :-
     case(_131248).

