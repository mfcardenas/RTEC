initially(status(_131576)=null).

initiatedAt(status(_131261)=proposed, _131292, _131249, _131294) :-
     happensAtIE(propose(_131269,_131261),_131249),_131249>=_131292,_131249<_131294,
     holdsAtCyclic(_131269,role_of(_131269,proposer)=true,_131249),
     holdsAtCyclic(_131261,status(_131261)=null,_131249).

initiatedAt(status(_131261)=voting, _131292, _131249, _131294) :-
     happensAtIE(second(_131269,_131261),_131249),_131249>=_131292,_131249<_131294,
     holdsAtCyclic(_131269,role_of(_131269,seconder)=true,_131249),
     holdsAtCyclic(_131261,status(_131261)=proposed,_131249).

initiatedAt(status(_131261)=voted, _131292, _131249, _131294) :-
     happensAtIE(close_ballot(_131269,_131261),_131249),_131249>=_131292,_131249<_131294,
     holdsAtCyclic(_131269,role_of(_131269,chair)=true,_131249),
     holdsAtCyclic(_131261,status(_131261)=voting,_131249).

initiatedAt(status(_131261)=null, _131293, _131249, _131295) :-
     happensAtIE(declare(_131269,_131261,_131271),_131249),_131249>=_131293,_131249<_131295,
     holdsAtCyclic(_131269,role_of(_131269,chair)=true,_131249),
     holdsAtCyclic(_131261,status(_131261)=voted,_131249).

initiatedAt(voted(_131261,_131262)=null, _131293, _131249, _131295) :-
     happensAtIE(vote(_131261,_131262,null),_131249),_131249>=_131293,_131249<_131295,
     holdsAtCyclic(_131261,role_of(_131261,voter)=true,_131249),
     holdsAtCyclic(_131262,status(_131262)=voting,_131249).

initiatedAt(voted(_131261,_131262)=aye, _131293, _131249, _131295) :-
     happensAtIE(vote(_131261,_131262,aye),_131249),_131249>=_131293,_131249<_131295,
     holdsAtCyclic(_131261,role_of(_131261,voter)=true,_131249),
     holdsAtCyclic(_131262,status(_131262)=voting,_131249).

initiatedAt(voted(_131261,_131262)=nay, _131293, _131249, _131295) :-
     happensAtIE(vote(_131261,_131262,nay),_131249),_131249>=_131293,_131249<_131295,
     holdsAtCyclic(_131261,role_of(_131261,voter)=true,_131249),
     holdsAtCyclic(_131262,status(_131262)=voting,_131249).

initiatedAt(voted(_131261,_131262)=null, _131271, _131249, _131273) :-
     initiatedAt(status(_131262)=null,_131271,_131249,_131273).

initiatedAt(outcome(_131261)=_131259, _131292, _131249, _131294) :-
     happensAtIE(declare(_131269,_131261,_131259),_131249),_131249>=_131292,_131249<_131294,
     holdsAtProcessedSimpleFluent(_131269,role_of(_131269,chair)=true,_131249),
     holdsAtProcessedSimpleFluent(_131261,status(_131261)=voted,_131249).

initiatedAt(per(vote(_131263,_131264))=true, _131285, _131249, _131287) :-
     initiatedAt(status(_131264)=voting,_131285,_131249,_131287),
     holdsAtCyclic(_131263,role_of(_131263,voter)=true,_131249).

initiatedAt(per(vote(_131263,_131264))=false, _131273, _131249, _131275) :-
     initiatedAt(status(_131264)=voted,_131273,_131249,_131275).

initiatedAt(per(vote(_131263,_131264))=false, _131272, _131249, _131274) :-
     happensAtIE(vote(_131263,_131264,_131271),_131249),
     _131249>=_131272,
     _131249<_131274.

initiatedAt(per(close_ballot(_131263,_131264))=true, _131296, _131249, _131298) :-
     initiatedAt(auxPerCloseBallot(_131264)=false,_131296,_131249,_131298),
     holdsAtProcessedSimpleFluent(_131264,status(_131264)=voting,_131249),
     holdsAtProcessedSimpleFluent(_131263,role_of(_131263,chair)=true,_131249).

initiatedAt(per(close_ballot(_131263,_131264))=false, _131273, _131249, _131275) :-
     initiatedAt(status(_131264)=voted,_131273,_131249,_131275).

initiatedAt(per(declare(_131263,_131264,_131265))=false, _131274, _131249, _131276) :-
     initiatedAt(status(_131264)=null,_131274,_131249,_131276).

initiatedAt(per(declare(_131263,_131264,_131265))=false, _131275, _131249, _131277) :-
     initiatedAt(role_of(_131263,chair)=false,_131275,_131249,_131277).

initiatedAt(obl(declare(_131263,_131264,_131265))=false, _131280, _131249, _131282) :-
     initiatedAt(per(declare(_131263,_131264,_131265))=false,_131268,_131249,_131270).

initiatedAt(role_of(_131261,voter)=false, _131286, _131249, _131288) :-
     happensAtIE(vote(_131261,_131271,_131272),_131249),_131249>=_131286,_131249<_131288,
     \+holdsAtCyclic(_131261,per(vote(_131261,_131271))=true,_131249).

initiatedAt(status(_131288)=null, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(status(_131288)=null).

initiatedAt(status(_131288)=proposed, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(status(_131288)=proposed).

initiatedAt(status(_131288)=voting, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(status(_131288)=voting).

initiatedAt(status(_131288)=voted, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(status(_131288)=voted).

initiatedAt(role_of(_131288,_131289)=true, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(role_of(_131288,_131289)=true).

initiatedAt(role_of(_131288,_131289)=false, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(role_of(_131288,_131289)=false).

initiatedAt(voted(_131288,_131289)=null, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(voted(_131288,_131289)=null).

initiatedAt(voted(_131288,_131289)=aye, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(voted(_131288,_131289)=aye).

initiatedAt(voted(_131288,_131289)=nay, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(voted(_131288,_131289)=nay).

initiatedAt(per(vote(_131290,_131291))=true, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(per(vote(_131290,_131291))=true).

initiatedAt(per(vote(_131290,_131291))=false, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(per(vote(_131290,_131291))=false).

initiatedAt(per(declare(_131290,_131291,_131292))=true, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(per(declare(_131290,_131291,_131292))=true).

initiatedAt(per(declare(_131290,_131291,_131292))=false, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(per(declare(_131290,_131291,_131292))=false).

initiatedAt(obl(declare(_131290,_131291,_131292))=true, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(obl(declare(_131290,_131291,_131292))=true).

initiatedAt(obl(declare(_131290,_131291,_131292))=false, _131249, _131250, _131251) :-
     prevTimePoint(0,_131250),
     _131249=<_131250,
     _131250<_131251,
     initially(obl(declare(_131290,_131291,_131292))=false).

initiatedAt(per(vote(_131267,_131268))=false, _131249, _131250, _131251) :-
     initiatedAt(role_of(_131267,voter)=false,_131249,_131250,_131251).

initiatedAt(auxPerCloseBallot(_131265)=true, _131249, _131250, _131251) :-
     initiatedAt(status(_131265)=voting,_131249,_131250,_131251).

initiatedAt(auxPerCloseBallot(_131265)=false, _131249, _131250, _131251) :-
     initiatedAt(status(_131265)=proposed,_131249,_131250,_131251).

initiatedAt(per(close_ballot(_131267,_131268))=false, _131249, _131250, _131251) :-
     initiatedAt(role_of(_131267,chair)=false,_131249,_131250,_131251).

initiatedAt(per(declare(_131267,_131268,carried))=true, _131249, _131250, _131251) :-
     initiatedAt(status(_131268)=voted,_131249,_131250,_131251),
     holdsAtCyclic(_131267,role_of(_131267,chair)=true,_131250),
     findall(_131299,user:(agent(_131299),holdsAt(voted(_131299,_131268)=aye,_131250)),_131301),
     length(_131301,_131324),
     findall(_131299,user:(agent(_131299),holdsAt(voted(_131299,_131268)=nay,_131250)),_131331),
     length(_131331,_131354),
     _131324>=_131354.

initiatedAt(per(declare(_131267,_131268,not_carried))=true, _131249, _131250, _131251) :-
     initiatedAt(status(_131268)=voted,_131249,_131250,_131251),
     holdsAtCyclic(_131267,role_of(_131267,chair)=true,_131250),
     findall(_131299,user:(agent(_131299),holdsAt(voted(_131299,_131268)=aye,_131250)),_131301),
     length(_131301,_131324),
     findall(_131299,user:(agent(_131299),holdsAt(voted(_131299,_131268)=nay,_131250)),_131331),
     length(_131331,_131354),
     _131324<_131354.

initiatedAt(obl(declare(_131267,_131268,_131269))=true, _131249, _131250, _131251) :-
     initiatedAt(per(declare(_131267,_131268,_131269))=true,_131249,_131250,_131251).

initiatedAt(obl(declare(_131267,_131268,_131269))=false, _131249, _131250, _131251) :-
     happensAtIE(declare(_131267,_131268,_131269),_131250),
     _131249=<_131250,
     _131250<_131251.

initiatedAt(sanctioned(_131265)=true, _131249, _131250, _131251) :-
     happensAtIE(close_ballot(_131265,_131274),_131250),
     _131249=<_131250,
     _131250<_131251,
     \+holdsAtProcessedSimpleFluent(_131265,per(close_ballot(_131265,_131274))=true,_131250).

initiatedAt(role_of(_131265,chair)=false, _131249, _131250, _131251) :-
     initiatedAt(status(_131279)=null,_131249,_131250,_131251),
     holdsAtCyclic(_131265,obl(declare(_131265,_131279,carried))=true,_131250),
     \+happensAtIE(declare(_131265,_131279,carried),_131250).

initiatedAt(role_of(_131265,chair)=false, _131249, _131250, _131251) :-
     initiatedAt(status(_131279)=null,_131249,_131250,_131251),
     holdsAtCyclic(_131265,obl(declare(_131265,_131279,not_carried))=true,_131250),
     \+happensAtIE(declare(_131265,_131279,not_carried),_131250).

terminatedAt(outcome(_131261)=_131259, _131270, _131249, _131272) :-
     initiatedAt(status(_131261)=proposed,_131270,_131249,_131272).

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

cachingOrder2(_131248, status(_131248)=null) :-
     motion(_131248).

cachingOrder2(_131248, status(_131248)=proposed) :-
     motion(_131248).

cachingOrder2(_131248, status(_131248)=voting) :-
     motion(_131248).

cachingOrder2(_131248, status(_131248)=voted) :-
     motion(_131248).

cachingOrder2(_131248, role_of(_131248,_131249)=true) :-
     agent(_131248),role(_131249).

cachingOrder2(_131248, voted(_131248,_131249)=aye) :-
     agent(_131248),motion(_131249).

cachingOrder2(_131248, voted(_131248,_131249)=nay) :-
     agent(_131248),motion(_131249).

cachingOrder2(_131250, per(vote(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, per(declare(_131250,_131251,carried))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, per(declare(_131250,_131251,not_carried))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, obl(declare(_131250,_131251,carried))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, obl(declare(_131250,_131251,not_carried))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131248, outcome(_131248)=carried) :-
     motion(_131248).

cachingOrder2(_131248, outcome(_131248)=not_carried) :-
     motion(_131248).

cachingOrder2(_131248, auxPerCloseBallot(_131248)=true) :-
     motion(_131248).

cachingOrder2(_131250, pow(propose(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, pow(second(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, pow(vote(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, pow(close_ballot(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, pow(declare(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, per(propose(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, per(second(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131250, per(close_ballot(_131250,_131251))=true) :-
     agent(_131250),motion(_131251).

cachingOrder2(_131248, sanctioned(_131248)=true) :-
     agent(_131248).

