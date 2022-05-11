filter([],_,[]).                                
filter([X|Xs],G,[X|Ys]):- Z=..[G,X], Z, filter(Xs,G,Ys).
filter([_|Xs],G,Ys):- filter(Xs,G,Ys).

g(X):- X<10.
/*
(1) filter([],_,[]).                                
(2) filter([X|Xs],G,[X|Ys]):- 
    (2.1) Z=..[G,X], 
    (2.2) Z, !,
    (2.3) filter(Xs,G,Ys).
(3) filter([_|Xs],G,Ys):-
    (3.1) filter(Xs,G,Ys).

(4) g(X):- 
    (4.1) X<10.
goal: 
filter([1,23,9,22],g,L)
unify with (1) fail!
unify with (2) success {1/X1, [23,9,22]/Xs1, g/G1,[1|Ys1]/L}
  body (2.1) : Z1 = ..[G1,X1] {g/G1, 1/X1, g(1)/Z1}
    = Z1 = g(1)  
  body (2.2) : Z1, !, {g(1)/Z1}
    = g(1), !
    unify with (4) success {1/X}
      body (4.1) : 1 < 10 **true
  body (2.3) : filter(Xs1,G1,Ys1) {[23,9,22]/Xs1, g/G1, Ys1/Ys1}
  = filter([23,9,22], g, Ys1)
  unify with (1) fail!
  unify with (2) success {23/X2, [9,22]/Xs2, g/G2, [23|Ys2]/Ys1}
    body (2.1) : Z2 = ..[G2, X2] {g/G2, 23/X2, g(23)/Z2}
      = Z2 = g(22)
    body (2.2) : Z2, !, {g(23)/Z2}
      = g(23), !
      unify with (4) success {23/X}
        body (4.1) : 23 < 10 **false
  unify with (3) success {23/_, [9,22]/Xs2, g/G2, Ys2/Ys1}
    body (3.1) : filter(Xs2,G2,Ys2)
      = filter([9,22],g,Ys2)
      unify with (1) fail!
      unify with (2) success {9/X3, [22]/Xs3, g/G3, [9|Ys3]/Ys2}
        body (2.1) : Z3 = ..[G3, X3]
          = Z3 = g(9)
        body (2.2) : Z3, !, {g(9)/Z3}
          = g(9), !
          unify with (4) success {g(9)/Z3}
            body (4.1) : 9 < 10 **true
        body (2.3) : filter(Xs3, G3, Ys2) {[22]/Xs3, g/G3, Ys2}
        = filter([22],g,Ys2)
        unify with (1) fail!
        unify with (2) success {22/X4, []/Xs4, g/G4,[22|Ys3]/Ys2}
          body (2.1) : Z4 = ..[G4, X4]
            = Z4 = g(22)
          body (2.2) : Z4, !, {g(22)/Z4}
            = g(22), !
            unify with (4) success {g(22)/Z4}
              body (4.1) : 22 < 10 ** false
        unify with (3) success {22/_, []/Xs4, g/G4, Ys3/Ys2}  
          body (3.1) : filter(Xs4,G,Ys3)
          = filter([],g,Ys3).
          unify with (1) success {[]/[], g/_, []/Ys3}
            body empty : answer **L = [1|Ys1] Ys1 = Ys2 Ys2 =[9|Ys3] Ys3 = []
                                  L = [1,9]
            

answer:
L = [1,9].
        
*/

%map(fun(X,Q,Y),Xs,Ys):-
 % findall(Y,(member(X,Xs),Q),Ys).%X is a member of Xs and satisfy Q predicate
map(fun(X,Q,Y), Xs, Ys) :- findall(Y, (member(X,Xs), Q), Ys ).
/*
edge(a,b).
edge(a,a).
edge(a,c).
edge(c,b).
edge(a,d).
edge(a,f).
edge(f,d).
edge(d,f).

mystery(L,Edges):-
    findall(edge(X,Y),(member(X,L),member(Y,L)),Edges),
    check(Edges).

check([]).
check([edge(X,X)|Rest]):- 
     check(Rest).
check([edge(X,Y)|Rest]):- 
    (edge(X,Y);edge(Y,X)),
     check(Rest).


resist(seq(C1,C2),R) :- resist(C1,R1),resist(C2,R2), R is (R1 + R2).
resist(par(C1,C2),R) :- resist(C1,R1),resist(C2,R2), R is ( (R1 * R2 ) / (R1 + R2)).
resist(res(X),R) :- R is X.


%main function
descend([X|Xs],Ys):-
   helper([[X]],Xs,Ys).


check(Xss,Y,Zs):-
  %add to last sublist
   last(Xs,Xss),
   last(X,Xs),
   X>=Y,
   append(Xs,[Y],Ws),
   rm_last(Xss,NewXss),
   append(NewXss,[Ws],Zs),!;
  %create a new sublist 
   last(Xs,Xss),
   last(X,Xs),
   X<Y,
   append(Xss,[[Y]],Zs).

%remove the last element of a list
rm_last([_], []):-!.
rm_last([X|Xs], [X|Ys]) :- 
   rm_last(Xs, Ys).
%find last element of a list
last(X,[X]):-!.
last(X,[_|Y]) :- last(X,Y).
%recursive function, pass one element to result list every recursion
helper(Xs,[],Xs):-!.%base case
helper(Xs,[Y|Ys],Zs):-
   check(Xs,Y,Ws),
   helper(Ws,Ys,Zs).%recursive call

*/

descend([],[]) :- !.
descend([X|XS],[Y|YS]) :- myde([X|XS],Y,R), descend(R,YS), !.   % Y is the first list calculate out, R is rest part of the list

myde([X,K|Rest],[X|YS],Z) :- (K < X),!, myde([K|Rest],YS,Z). %if X > K, then append K to the front of the list, to make the next comparison.
myde([X,K|Rest],[X],[K|Rest]) :- (X < K).  %if X < K, then append K to the front of return list, since it does not fits the requirement
myde(X,X,[]). %base case


descend([],[]).
descend(X,[Y|YS]) :- de2(X,Y,[],Z), descend(Z,YS).

de2([],[],_,[]).
de2([X|XS],[X|YS],K,Z) :- compare(K,X),!, de2(XS,YS,X,Z).
de2([X|XS],[],K,[X|XS]):- compare(X,K).

compare([],_).
compare(_,[]).
compare(X,Y) :- X>Y.

%resist(seq(par(res(4),res(5))),res(2),R).

%resist(seq(C1,C2),R) :- resist(C1,R1),resist(C2,R2), R is (R1 + R2).
%resist(par(C1,C2),R) :- resist(C1,R1),resist(C2,R2), R is ( (R1 * R2 ) / (R1 + R2)).
%resist(res(X),X).

resist(res(N),N).
resist(seq(C1,C2),Return) :- resist(C1,R1),resist(C2,R2), Return is (R1 + R2).
resist(par(C1,C2),Return) :- resist(C1,R1),resist(C2,R2), Return is ((R1 * R2 )/(R1 + R2)).


/*
(1) filter([],_,[]).                                
(2) filter([X|Xs],G,[X|Ys]):- 
    (2.1) Z=..[G,X], 
    (2.2) Z, !,
    (2.3) filter(Xs,G,Ys).
(3) filter([_|Xs],G,Ys):-
    (3.1) filter(Xs,G,Ys).

(4) g(X):- 
    (4.1) X<10.
goal: 
filter([1,23,9,22],g,L)
unify with (1) fail!
unify with (2) success {1/X1, [23,9,22]/Xs1, g/G1,[1|Ys1]/L}
  body (2.1) : Z1 = ..[G1,X1] {g/G1, 1/X1, g(1)/Z1}
    = Z1 = g(1)  
  body (2.2) : Z1, !, {g(1)/Z1}
    = g(1), !
    unify with (4) success {1/X}
      body (4.1) : 1 < 10 **true
  body (2.3) : filter(Xs1,G1,Ys1) {[23,9,22]/Xs1, g/G1, Ys1/Ys1}
  = filter([23,9,22], g, Ys1)
  unify with (1) fail!
  unify with (2) success {23/X2, [9,22]/Xs2, g/G2, [23|Ys2]/Ys1}
    body (2.1) : Z2 = ..[G2, X2] {g/G2, 23/X2, g(23)/Z2}
      = Z2 = g(22)
    body (2.2) : Z2, !, {g(23)/Z2}
      = g(23), !
      unify with (4) success {23/X}
        body (4.1) : 23 < 10 **false
  unify with (3) success {23/_, [9,22]/Xs2, g/G2, Ys2/Ys1}
    body (3.1) : filter(Xs2,G2,Ys2)
      = filter([9,22],g,Ys2)
      unify with (1) fail!
      unify with (2) success {9/X3, [22]/Xs3, g/G3, [9|Ys3]/Ys2}
        body (2.1) : Z3 = ..[G3, X3]
          = Z3 = g(9)
        body (2.2) : Z3, !, {g(9)/Z3}
          = g(9), !
          unify with (4) success {g(9)/Z3}
            body (4.1) : 9 < 10 **true
        body (2.3) : filter(Xs3, G3, Ys3) {[22]/Xs3, g/G3, [X3|Ys3]/Ys2}
        = filter([22],g,Ys3)
        unify with (1) fail!
        unify with (2) success {22/X4, []/Xs4, g/G4,[22|Ys4]/Ys3}
          body (2.1) : Z4 = ..[G4, X4]
            = Z4 = g(22)
          body (2.2) : Z4, !, {g(22)/Z4}
            = g(22), !
            unify with (4) success {g(22)/Z4}
              body (4.1) : 22 < 10 ** false
        unify with (3) success {22/_, []/Xs4, g/G4, Ys4/Ys3}  
          body (3.1) : filter(Xs4,G,Ys4)
          = filter([],g,Ys4).
          unify with (1) success {[]/[], g/_, Ys3/[]}
            body empty : answer **L = [1|Ys1] Ys1 = Ys2 Ys2 =[9|Ys3] Ys3 = Ys4 Ys4 = []
                                  L = [1,9]
            

answer:
L = [1,9].
 
*/