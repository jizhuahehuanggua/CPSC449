% -*- Mode:Prolog -*-
%  (the above is for the emacs editor ...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   This is a runnable prolog file: try running me in prolog!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/******************************************************************

                  REVERSING and PERMUTING

Some basic very useful predicates are for reversing a list and finding all permutations of as list.

Let us start by considering the problem of reversing a list.  A simple program 
follows the naive reverse algorithm.  
******************************************************************/
append([],X,X).
append([H|T],X,[H|Y]):-
       append(T,X,Y).

reverse([],[]).
reverse([H|T],Y):-
       reverse(T,W),
       append(W,[H],Y).

/******************************************************************
Here we are thinking of the reverse predicate as taking an input 
in the first argument ("instantiating" the variable in the first 
argument) and providing output in the second argument.  This may 
be expressed by: 

           reverse(+,-)

However we may want also the reverse behavior:

           reverse(-,+)

where we instantiate the secondargument and hope for output in the 
first argment. The ambiguity on direction can be expressed by:

           reverse(?,?)

So what does happen it we run this code backwards?

          reverse(Z,[a,b,c]) 

Initially gives Z=[c,b,a], however, if you ask it for other 
alternatives it disappears into an infinite recursion.  This 
is potentially quite dangerous as if one backtracks over this 
pedicate it will cause non-termination.

What can be done?

One answer is to carefully ony use the predicate in the 
forward mode ... giving no guarantees for what happens when you 
run it backward.

A stab at an answer -- an interesting suggestion from class -- is 
to employ a strategically placed "cut" to stop the backtracking:
******************************************************************/

reverse1([],[]).
reverse1([H|T],Y):-
       reverse1(T,W),
       append(W,[H],Y),!.

/******************************************************************

Try:
 
    reverse1(Z,[a,b,c]).

Unfortunately you will discover this returns "false."  So what is happening?
Well the second line calls reverse and initially matches reverse1([],[]).
However, this means append([],[H],[a,b,c]) is called: this fails. So 
reverse(Z,[a,b,c]) is called again. Backtracking and using the second rule 
gives succesfully revers1([H'],[H']) which fails on calling 
append([H'],[H],[a,b,c])!

The trouble is the intermediate reverse has to generate attemps at reversing 
variable list ... but as its behaviour has been made deterministic after 
its first attempt it refuses to try anything else due to the cut.

OK so this is a sort of WARNING: you have to think through placing cuts 
very carefully.  Better to avoid them!

OK so how do we do that? Well lets not give up on cuts. Try reverse2:
******************************************************************/

reverse2(X,Y):-
    reverse(X,Y),!.

/******************************************************************

OK so this now works forward and backwards. When it works backwards it
is a little inefficient as recall there is some internal failing to get 
to the right length intermediate list. 

However now try:
      reverse2(X,Y)
it returns immediately with X=Y, Y=[] with no altenatives.

Finally consider:
******************************************************************/


reverse3(X,Y):-
    samelength(X,Y),
    reverse(X,Y).

samelength([],[]).
samelength([_|X],[_|Y]):-
    samelength(X,Y).

/******************************************************************
As we know when we reverse a list we must get a list of the same 
length we can force this and then call our original reverse.

This now works backwards and forward AND if you try:
      reverse3(X,Y)
it returns X=Y,Y=[] and if you force backtracking (with ";") prolog 
returns X = Y, Y = [_2976], if you force backtracking again prolog returns
X=[_2976,_2882],Y = [_2882,_2976].  Thus, it starts reversing lists of 
variables of increasing length ...

While this is not extremely efficient we do now have a nice predicable 
behavior! 

Now we also know there is a fast reverse.  So let us program that in 
prolog:
******************************************************************/

frev(X,Y):-
    shunt(X,[],Y).

shunt([],X,X).
shunt([H|T],S,Z):-
    shunt(T,[H|S],Z).

/*******************************************************************
Some thought about this makes one realize that this is a 
          frev(+,-)
predicate.  However again we can make it behave well in both directions 
using the trick of checking for the same length.  This gives:
*********************************************************************/

freverse(X,Y):-
    samelength(X,Y),
    frev(X,Y).

/*******************************************************************  
That's an awful lot about reversing!!

A more interesting predicate is permutation: this is the predicate which 
is true when its two arguments are permutations of each other.  Here is 
our first shot at it:
*********************************************************************/

perm([],[]).
perm([H|T],Z):-
    perm(T,S),
    insert(H,S,Z).

insert(H,T,[H|T]).
insert(H,[X|T],[X|Z]):-
    insert(H,T,Z).

/*******************************************************************  
Some thought about this and one realizes this is a 
          perm(+,-)
predicate but we can use the same trick to secure a good two way 
behaviour:
*********************************************************************/

permutation(X,Y):-
    samelength(X,Y),
    perm(X,Y).

/*******************************************************************  
Here is a "very prolog" way to sort a list of numbers: try all permutations 
until a sorted list is found!  
*********************************************************************/

permsort(X,Y):-
    permutation(X,Y),
    sorted(Y).

sorted([]).
sorted([_]).
sorted([X,Y|Rest]):-
    X =< Y,sorted([Y|Rest]).

/********************************************************************
This is called "permutation sort".  It is very inefficient ... as 
in the worst case it takes O(n!).

Try testing this with
       sort([3,1,73,2,9],Z)

this is a two way predicate: what is its backward behavior?

A more efficient sorting is a merge sort.  Here it is but it is definitely
a mergesort(+,-) thing!
*********************************************************************/

mergesort([],[]).
mergesort([X],[X]).
mergesort(X,Y):-
    split(X,X1,X2),
    mergesort(X1,Y1),!,
    mergesort(X2,Y2),!,
    merge(Y1,Y2,Y).

split([],[],[]).
split([H],[H],[]).
split([H1,H2|Rest],[H1|R1],[H2|R2]):-
    split(Rest,R1,R2).

merge([],[],[]).
merge([],L,L).
merge(L,[],L).
merge([H1|T1],[H2|T2],[H2|Y]):-
    H2 =< H1,
    merge([H1|T1],T2,Y).
merge([H1|T1],[H2|T2],[H1|Y]):-
    H1 < H2,
    merge(T1,[H2|T2],Y).
   