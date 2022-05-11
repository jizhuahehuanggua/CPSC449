% Instructions:
%   - Please submit ONLY this file up to GradeScope!
%   - Implement as many of the predicates as you can as specified in the assignment!
%   - Have lots of fun! Wahoo!
%   - Do not change the module information of this file or the name of this file (i.e., do not change line 6)!
:- module( ex3, [ myappend/3 , myreverse/2 , myflatten/2 , mymember/2 , myremove/3 , mymember2/2 , mysubstring/2 , mysublists/2 , mypermutation/2 , grandfather/2 , grandmother/2 , brother/2 , sister/2 , sibling/2 , cousin/2 , contact/3 , josephus/5 ]).


%%%% Question 1.
myappend([],Y,Y).
myappend([X|XS], Y, [X|ZS]) :- myappend(XS,Y,ZS).
        


%naive reverse
%myreverse([],[]).
%YS = ZS - reverse(XS) ++ [X]
%myreverse([X|XS],YS) :- myreverse(XS, ZS), myappend(ZS,X,YS) 
                        %ZS = reverse(XS), YS = ZS ++ [X]
%this is from Xi Wang T03T04. 
myreverse(XS,YS) :- sos(XS, [], YS, YS), !.

sos([], ZS, ZS, []).
sos([X|XS], LS, RS, [_|YS]) :- sos(XS, [X|LS], RS, YS).


%insert(X,Y,[X|Y]).
%insert(X,[H|Y],[H|Z]) :- insert([X|H],Y,Z).

%if the element is a list, then flattent the rest, and append the current element with the return value from the rest of the list
%if the first element is an element, we skip
myflatten([],[]).
myflatten([X|XS],YS) :- isList(X), myflatten(XS,Z), myappend(X,Z,YS), !.
%myflatten([[X|XS]|XSS],YS) :- myflatten(XSS,Z), myappend([X|XS],Z,YS), !.
myflatten([X|XS],[X|YS]) :- myflatten(XS,YS).

isList([]).
isList([X|XS]) :- isList(XS).

%this is from lecture note
%if meet the target return true
%otherwise continue comparison or return false
mymember(X, [X|_]).
mymember(X, [_|YS]) :- mymember(X,YS).

%remove X from Y
%if meet the remove target return rest of the list
myremove(X,[X|XS],XS).
%if it is not the target, add it to the front of the return list and continue do the comparison
myremove(X,[Y|YS],[Y|ZS]) :- myremove(X,YS,ZS).


%%%% Question 2.
%N is a couter, starts from 0,whenever it meets X, n + 1.
%if its not  equals to 2, then false.

%set a counter to count how manny times X appear, function will be true when there are exactly two X appears in the list
mymember2(X,Y) :- mymemberhelper(X,Y,N), N is 2.  

mymemberhelper(X,[],0).
mymemberhelper(X,[X|YS],N) :- mymemberhelper(X,YS, Z), N is Z + 1, !.
mymemberhelper(X,[Y|YS],N) :- mymemberhelper(X,YS,N), !.


%%%% Question 3.
%the algorithm is based on the first exercise question11
%compare each char, if char x == string[b] y then keep comparing untill the first string is done
%if a != b repeat comparison a with string[b+1]
mysubstring([],_).     
mysubstring([X|XS],[X|YS]) :- subsubstring(XS,YS).                                     
mysubstring([X|XS],[Y|YS]) :- mysubstring([X|XS],YS).

subsubstring([],Y).
subsubstring([X|XS],[X|YS]) :- subsubstring(XS,YS).

%%%% Question 4
%this separate each element from input list
%concat each element into front of the rest of elements
%then append all out comes into one list
mysublists([],[[]]).
mysublists([X|XS],Y) :- mysublists(XS,SBlist),myconcat(X,SBlist,SBSBlist) ,myappend(SBlist,SBSBlist,Y).

myconcat(_, [], []).
myconcat(X, [Y|YS], [[X|Y]|KN]) :- myconcat(X,YS,KN).

%%%% Question 5
%from textbook page 161.
mypermutation([], []).
mypermutation(L, [H|T]) :- append(V, [H|U], L), append(V, U, W), mypermutation(W, T).

%%%% Question 8

% Understand these predicates as follows.
%   son(Mom, Dad, Child)      is read as ``Child is the son of the mother Mom and the father Dad''
%   daughter(Mom, Dad, Child) is read as ``Child is the daughter of the mother Mom and the father Dad''

%son(mymom, mydad, theson).
%daughter(mymom, mydad, thedaughter).
% Add your own family members too!

% Understand these predicates as follows.
%   grandfather(A,B). is read as ``A is a grandfather of B''
%   grandmother(A,B). is read as ``A is a grandmother of B''
%   brother(A,B).     is read as ``A is a brother of B''
%   sister(A,B).      is read as ``A is a sister of B''
%   sibling(A,B).     is read as ``A is a sibling of B''
%   cousin(A,B).      is read as ``A is a cousin of B''

grandfather(A,B) :- 
                    (daughter(_,A,C),son(C,_,B));
                    (son(_,A,C),son(_,C,B));
                    (daughter(_,A,C),daughter(C,_,B));
                    (son(_,A,C),daughter(_,C,B)).
grandmother(A,B) :-
                    (daughter(A,_,C),son(C,_,B));
                    (son(A,_,C),son(_,C,B));
                    (daughter(A,_,C),daughter(C,_,B));
                    (son(A,_,C),daughter(_,C,B)).
brother(A,B) :-
                    (son(F,M,A),son(F,M,B));
                    (son(F,M,A),daughter(F,M,B)).
                    
sister(A,B) :-
                    (daughter(F,M,A),son(F,M,B));
                    (daughter(F,M,A),son(F,M,B)).
sibling(A,B) :-
                    (daughter(F,M,A),son(F,M,B));
                    (daughter(F,M,A),son(F,M,B)); 
                    (son(F,M,A),son(F,M,B));
                    (son(F,M,A),daughter(F,M,B)).

cousin(A,B) :-  (grandfather(GF,A),grandfather(GF,B),not(brother(A,B)),not(sister(A,B)));
                (grandmother(GM,A),grandmother(GM,B),not(brother(A,B)),not(sister(A,B))).



%%%% Question 7
%pseudo-code is from Xi Wang T03T04
%requirements:
%0<= StartTime - MeetTime <= 7 
%meet = met(starter, meetTime, Mid) ; met(Mid, MeetTime, Starter)
%all needs to fit above requirements
[met('Aspen12598',5,'Bay12598'),met('Bobbie12598',19,'Brooklyn12598'),met('Blake12598',9,'Corey12598'),met('Corey12598',29,'Charlie12598'),met('Blair12598',6,'Adrian12598'),met('Dakota12598',17,'Aspen12598'),met('Brooklyn12598',30,'Dakota12598'),met('Bay12598',11,'Brooklyn12598'),met('Cameron12598',4,'Ash12598'),met('Dakota12598',27,'Charlie12598'),met('Blake12598',19,'Amari12598'),met('Amari12598',26,'Blake12598'),met('Blake12598',20,'Adrian12598'),met('Charlie12598',12,'Aspen12598'),met('Carson12598',12,'Ariel12598'),met('Amari12598',8,'Aspen12598'),met('Amari12598',12,'Charlie12598'),met('Bobbie12598',18,'Charlie12598'),met('Adrian12598',16,'Dallas12598'),met('Cameron12598',3,'Blair12598'),met('Blake12598',11,'Blair12598'),met('Bay12598',19,'Dallas12598'),met('Blair12598',20,'Ariel12598'),met('Aspen12598',15,'Adrian12598'),met('Ariel12598',13,'Blair12598'),met('Brooklyn12598',13,'Adrian12598'),met('Adrian12598',20,'Corey12598'),met('Bobbie12598',19,'Corey12598'),met('Bay12598',18,'Bobbie12598')].
%contact([],[],[]).
%base case: if the meeting time for the last pair is >= 0 and <=7, they infect each other
%basecase: startPerson = GoalPerson
contact(infect(Starter, StartTime),infect(Target,TargetTime),Path) :-
    (met(Starter,MeetTime,Target); met(Target,MeetTime,Starter)),
    (StartTime - MeetTime) =< 7, (MeetTime - TargetTime) =< 7, %check meeting time
    0 =< (StartTime - MeetTime), 0 =< (MeetTime - TargetTime) ,
    Path = [infect(Target,MeetTime),infect(Starter,StartTime)], !.

%recursive step:
%startperson met MidMan, then figure out the path.
%MidMan ->GoalPerson && fit requirements
contact(infect(Starter,StartTime),infect(Target,TargetTime), Path) :-
    (met(Target,MeetTime,MidMan); met(Target,MeetTime,Starter)),
    (StartTime - MeetTime) =< 7, (MeetTime - TargetTime) =< 7,
    0 =< (StartTime - MeetTime), 0 =< (MeetTime - TargetTime) , 
    append([infect(Target,MeetTime)],MidPart,Path),
    contact(infect(Starter,StartTime),infect(MidMan,MeetTime),MidPart),!.

%%%% Question 8
% Please see the assignment for the logic puzzle. 
% This question will just be hand graded!
%the format and the starter code are:
%from http://pages.cpsc.ucalgary.ca/~robin/class/449/notes/solvingPuzzles.txt
%and this question is based on examples from http://pages.cpsc.ucalgary.ca/~robin/class/449/notes/solvingPuzzles.txt
solver(Name,Solution):- get_puzzle(Name,P),solve_puzzle(P,Solution).

solve_puzzle(puzzle(Clues,Queries,Solution),Solution):-
     solve(Clues),
     solve(Queries).

solve([Clue|Clues]):- Clue,solve(Clues).
solve([]).

get_puzzle(Name,puzzle(Clues,Queries,Solution)):-
    structure(Name,Structure),
    clues(Name,Structure,Clues),
    queries(Name,Structure,Queries,Solution).

structure(housesss,[house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_)]).

% Helpful predicates!!
color(house(C,_,_,_,_),C).  % different color
nationality(house(_,N,_,_,_),N).    %owned by men of different nationalities
pet(house(_,_,P,_,_),P).       %own different pets
drink(house(_,_,_,D,_),D).  %prefer different drinks
sport(house(_,_,_,_,S),S).  %play a different sport

%A lives in the house next to the B
nextto(A,B,[B,A,_,_,_]).
nextto(A,B,[_,B,A,_,_]).
nextto(A,B,[_,_,B,A,_]).
nextto(A,B,[_,_,_,B,A]).
nextto(A,B,[A,B,_,_,_]).
nextto(A,B,[_,A,B,_,_]).
nextto(A,B,[_,_,A,B,_]).
nextto(A,B,[_,_,_,A,B]).

%A is immediately to the right of the B
rightof(A,B,[B,A,_,_,_]).
rightof(A,B,[_,B,A,_,_]).
rightof(A,B,[_,_,B,A,_]).
rightof(A,B,[_,_,_,B,A]). 

%A is the middle house.
inMiddle(A,[_,_,A,_,_]).
%A lives in the first house on the left
onLeft(A,[A,_,_,_,_]).

clues(housesss,House,
    [   
        %The Irishman lives in the first house on the left.
        %(onLeft(A,House),nationality(A,irishman)),
        (nationality(A,irishman),onLeft(A,House)),
        %The man who plays baseball lives in the house next to the man who keeps a tiger.
        (sport(B,baseball),nextto(B,C,House),pet(C,tiger)),
        %The occupant of the house, next to the house where the owner rides a horse, plays soccer.
        (nextto(D,E,House),sport(D,soccer),pet(E,horse)),
        %The squash player drinks gin
        (mymember(F,House),sport(F,squash),drink(F,gin)),
        %The Frenchman plays rugger
        (mymember(G,House),nationality(G,frenchman),sport(G,rugger)),
        %The Irishman lives next to the blue house.
        (nationality(H,irishman),color(I,blue),nextto(H,I,House)),
        %The Englishman lives in the red house.
        (nationality(J,engliblueshman),color(J,red),mymember(J,House)),
        % The Spaniard is often seen taking his dog for a walk.
        (mymember(K,House),nationality(K,spaniard),pet(K,dog)),
        % Beer is brewed (and drunk in large quantities) in the green house.
        (mymember(L,House),drink(L,beer),color(L,green)),
        %The Scotsman drinks whiskey and is often tipsy.
        (mymember(M,House),nationality(M,scotsman),drink(M,whiskey)),
        %The green house is immediately to the right of the white house
        (color(N,green),rightof(N,O,House),color(O,white)),
        % The tennis player owns snakes.
        (mymember(P,House),sport(P,tennis),pet(P,snakes)),
        %Soccer is played in the yellow house.
        (mymember(Q,House),sport(Q,soccer),color(Q,yellow)),
        %A lot of wine get consumed in the middle house.
        (drink(R,wine),inMiddle(R,House))
    ]).

queries(housesss,House,
    [
        %Who owns the hamster?
        mymember(OH,House), pet(OH,hamster), nationality(OH,P1),
        %Who drinks orange juice?
        mymember(OJ,House), drink(OJ,juice) , nationality(OJ,P2)
    ],
    [
        % The solution
        [P1, 'owns the hamster'],
        [P2, 'drinks orange juice']
    ]
).



%%%% Question 9
% The parameters are as follows...
%   - NumberOfSoldiers: the total number of soldiers including Josephus and his accomplice (> 2)
%   - StartingPosition: the starting position
%   - N: The selected number to count down
%   - J: Output position for Josephus (< NumberOfSoldiers)
%   - A: Output position for the accomplice (< NumberOfSoldiers)
% where all positions are 0 indexed
josephus(NumberOfSoldiers, StartingPosition, N, J, A) :-
    CurPos is mod(StartingPosition,NumberOfSoldiers),              %starting position = starting position mod number of soldiers
    MyCountDown is mod(N,NumberOfSoldiers),                         %count down is N mod number of solders
    myjose(NumberOfSoldiers,CurPos,MyCountDown,RT),                 %generate solder list and do the first kill, then return a list
    length(RT,MySoldNum),                                           % length of the list would be the number of solder whos still alive
    aLoop(RT,MySoldNum,N,[J,A]),!.                                  %loop. DO the kill action untill there are only two survivor

%a loop does the kill action untill there is only two person left
aLoop(SoldList,2,N,SoldList).
aLoop(SoldList, SoldNum,N,SurviveList) :-  
    MCountDown is mod(N,SoldNum),       %recalculate the countDown based on the amount of current survivor
    findNKill(SoldList,MCountDown,RT),  %find the person and kill
    NewSoldNum is SoldNum - 1,          %survivor -1, since one person has been killed.
    aLoop(RT,NewSoldNum,N,SurviveList). %looping

myjose(NumberOfSoldiers, CurPos, MyCountDown, R):- 
    %CurPos is mod(StartingPosition,NumberOfSoldiers),              %current position is starting position mode n
    %MyCountDown is mod(N,NumberOfSoldiers),
    CurPosM is CurPos - 1,                          %separate the list by two, to make it be the form of [first dead solder..last solder,1,2,3,...first dead solder]
    numlist(CurPos, NumberOfSoldiers,FirstList),
    numlist(1, CurPosM,SecondList), 
    append(FirstList,SecondList,RList), 
    findNKill(RList,MyCountDown,R).         %do the kill action

%find the solder and move it out of the list, 
%append all the person above him to the end of the list,
findNKill([X|XS],0,XS):-!. 
findNKill([X|XS], CountDown,Y) :- NewCountDown is CountDown - 1, append(XS,[X],XSS),findNKill(XSS,NewCountDown,Y).


feud(macbeth,duncan).
feud(macbeth,macduff).
feud(macdonald,cameron).
feud(alpin,bruce).
feud(macdougal,macduff).


%seating(InvitationList,SeatingPlan):- ...

%feud_eruption(SeatingPlan) :- feud_eruption_SOS(SeatingPlan).

feud_eruption([X,Y]) :- (feud(X,Y) ; feud(Y,X)).
feud_eruption([X,Y|XS]) :- (feud(X,Y) ; feud(Y,X)) ,feud_eruption([Y|XS]).
%feud_eruption([X,Y|XS]) :- feud_eruption([Y|XS]).

seating(X, [Y|[Ys|Yuk]]):- remove(Y,X,Next), remove(Ys,Next,Yss), not(feud(Y,Ys)),not(feud(Ys,Y)), seating(Next,[Ys|Yuk]).
seating([X|[]],[X|[]]) := !.

seating([X,Y|XS], [X,Y|XS]) :-  
% (1) remove(X,[X|Xs],Xs).
% (2) remove(X,[H|Y],[H|Ys]):- 
%     (2.1)remove(X,Y,Ys).

% goal: remove(X,[a,b],Y).
% 	unify with (1) remove(X1,[X1|Xs1],Xs1). success {X1/X, a/X1, [b]/Xs1, [b]/Y1}
% 		body empty : answer     **X = X1 = a, Y = [b]**; (force backtracking)
% 	unify with (2) remove(X1,[H1|Y1],[H1|Ys1]) success {X1/X, a/H1, [b]/Y1, [a|Ys1]/Y1}
% 		 body(2.1): remove(X1,Y1,Ys1). {X1/X, a/H1, [b]/Y1, [a|Ys1]/Y1}
% 			=remove(X1,[b],Ys1)
% 		 		unify with (1) remove(X2,[X2|Xs2],Xs2). success{b/X1, []/Xs2, []/Ys1}
% 					body empty : answer     **X1 = b, Y1 = [a|[]] = [a]; (force backtracking)
% 				unify with (2) remove(X,[H|Y],[H|Ys]). success {X2/X1, b/H2, []/Y2, [b|Ys2]/Ys1}
% 					body(2.1): remove(X2,Y2,Ys2). {X2/X1, b/H2, []/Y2, [b|Ys2]/Ys1}
% 						=remove(X, [], Ys2)
% 							unify with (1) remove(X3,[X3|Xs3],Xs3). fail!
%                                 					unify with (2) remove(X3,[H3|Y3],[H3|Ys3]). fail!
% 						False.

