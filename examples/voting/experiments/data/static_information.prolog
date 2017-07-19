

:- ['agent1000.prolog'].
%:- ['agent2000.prolog'].
%:- ['agent3000.prolog'].
%:- ['agent4000.prolog'].
%:- ['agent5000.prolog'].
%:- ['agent6000.prolog'].
%:- ['agent7000.prolog'].
%:- ['agent8000.prolog'].
%:- ['agent9000.prolog'].
%:- ['agent10000.prolog'].

%agent(1).
%agent(2).
%agent(3).
%agent(4).
%agent(5).
%agent(6).

role(chair).
role(voter).

initiatedAt(role_of(Ag,chair)=true, T1, -1, T2) :- T1=<(-1), -1<T2, Temp is Ag mod 50, Temp=0.
initiatedAt(role_of(Ag,voter)=true, T1, -1, T2) :- T1=<(-1), -1<T2.

motion(1).
motion(2).
motion(3).
motion(4).
motion(5).
motion(6).
motion(7).
motion(8).

