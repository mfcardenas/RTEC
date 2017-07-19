
:- ['../queries.prolog'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT: choose the appropriate number of static information 
% including agents and INITIAL ROLE ASSIGNMENT and stream:
:- ['../../data/static_information.prolog'].
%:- ['../../data/stream.prolog'].
:- ['data-generator.prolog'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% performFullER(WM+'.txt', WM, 1000, 49000).

performFullER(Filename, WM, Step, LastTime) :-
  open(Filename, write, Stream),
  initialiseRecognition(unordered, nopreprocessing, 1),
  updateManySDE(0, WM),
  WMPlus1 is WM+1, 
  write('ER: '),write(WM),write(WMPlus1),nl, 
  statistics(cputime,[S1,T1]), 
  eventRecognition(WM,WMPlus1), 
  findall((F=V,L), (outputEntity(F=V),holdsFor(F=V,L)), CC), 
  findall((EE,TT), (outputEntity(EE),happensAt(EE,TT)), ListofTimePoints),
  statistics(cputime,[S2,T2]), T is T2-T1, S is S2-S1, %S=T2,
  write(Stream, S),
  CurrentTime is WM+Step,
  updateManySDE(WM, CurrentTime),
  Diff is CurrentTime-WM,
  querying(Stream, WM, Step, CurrentTime, LastTime, [S], WorstCase),
  % calculate average query time
  sum_list(WorstCase, Sum),
  length(WorstCase, L),
  AvgTime is Sum/L,
  nl(Stream), write(Stream, AvgTime),
  % calculate max query time
  max_list(WorstCase, Max),
  nl(Stream), write(Stream, Max),
  close(Stream),!.

querying(_Stream, _WM, _Step, CurrentTime, LastTime, WorstCase, WorstCase) :- 
  CurrentTime > LastTime, 
  !.

querying(Stream, WM, Step, CurrentTime, LastTime, InitWorstCase, WorstCase) :- 
  write('ER: '),write(CurrentTime),write(' '),write(WM),nl,
  statistics(cputime,[S1,T1]), 
  eventRecognition(CurrentTime, WM), 
  findall((F=V,L), (outputEntity(F=V),holdsFor(F=V,L)), CC), 
  findall((EE,TT), (outputEntity(EE),happensAt(EE,TT)), ListofTimePoints),
  statistics(cputime,[S2,T2]), 
  T is T2-T1, S is S2-S1, %S=T2,
  writeResult(S, Stream),
  NewCurrentTime is CurrentTime+Step,
  updateManySDE(CurrentTime, NewCurrentTime),
  Diff is NewCurrentTime-WM,
  querying(Stream, WM, Step, NewCurrentTime, LastTime, [S|InitWorstCase], WorstCase).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I/O Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeResult(Time, Stream):-
  write(Stream,'+'), write(Stream,Time).



  

