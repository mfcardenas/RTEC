



updateSDE(Start, End) :-
      % propose motions
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 500, Temp=0, motion(M), Temp2 is M mod 2, Temp2=0, assert(happensAtIE(propose(Ag, M), Start))), _),
      StartPlus1 is Start+1,	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 480, Temp=0, motion(M), Temp2 is M mod 3, Temp2=0, assert(happensAtIE(propose(Ag, M), StartPlus1))), _),
      % close ballots
      StartPlus2 is Start+2,	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 500, Temp=0, motion(M), Temp2 is M mod 5, Temp2=0, assert(happensAtIE(close_ballot(Ag, M), StartPlus2))), _),
      % second the first batch of proposed motions
      StartPlus3 is Start+3,   
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 500, Temp=0, motion(M), Temp2 is M mod 2, Temp2=0, assert(happensAtIE(second(Ag, M), StartPlus3))), _),
      % voting
      StartPlus4 is Start+4, 	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 200, Temp=0, motion(M), Temp2 is M mod 2, Temp2=0, assert(happensAtIE(vote(Ag, M, aye), StartPlus4))), _),
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 101, Temp=0, motion(M), Temp2 is M mod 2, Temp2=0, assert(happensAtIE(vote(Ag, M, nay), StartPlus4))), _),
      % second all motions (some have not been proposed)
      %StartPlus5 is Start+5,   
      %findall((Ag,M),	(agent(Ag), Temp is Ag mod 600, Temp=0, motion(M), assert(happensAtIE(second(Ag, M), StartPlus5))), _),
      % voting
      StartPlus6 is Start+6, 	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 102, Temp=0, motion(M), Temp2 is M mod 4, Temp2=0, assert(happensAtIE(vote(Ag, M, aye), StartPlus6))), _),
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 3, Temp=0, motion(M), Temp2 is M mod 8, Temp2=0, assert(happensAtIE(vote(Ag, M, nay), StartPlus6))), _),
      % close ballots
      StartPlus7 is Start+7, 	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 100, Temp=0, motion(M), assert(happensAtIE(close_ballot(Ag, M), StartPlus7))), _),
      % declare results
      StartPlus8 is Start+8, 	
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 110, Temp=0, motion(M), assert(happensAtIE(declare(Ag, M, not_carried), StartPlus8))), _),
      StartPlus9 is Start+9,
      findall((Ag,M),	(agent(Ag), Temp is Ag mod 120, Temp=0, motion(M), assert(happensAtIE(declare(Ag, M, carried), StartPlus9))), _).


