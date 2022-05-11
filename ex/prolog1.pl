% -*- Mode:Prolog -*-
%  (the above is for the emacs editor ...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   This is a runnable prolog file: try running me in prolog!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/******************************************************************

Being able to hand evaluating a polog program is an important
skill.  It is crucial in the debugging process ... and some pretty
subtle errors can arise while using prolog.

We shall as an example hand evaluate the insert predicate.

*******************************************************************/

insert(X,Y,[X|Y]).
insert(X,[H|Y],[H|Z]) :- insert(X,Y,Z).

/******************************************************************

The first thing to realize is that this predicate can be used in various
different ways.  It enforces a relationship between the variables and
relationships can be run in all sorts of directions.

To realize this we instantiate (i.e. fill the variable with concrete
structures) invarious different ways:

*******************************************************************/

:-write("insert(x,[a,b],Z)"),nl,forall(insert(x,[a,b],Z),(write("Z ="),write(Z),nl)),nl.

/******************************************************************

Here if you press ";" to force backtracking to get all the possible
answers you will get:

Z=[x,a,b];
Z=[a,x,b];
Z=[a,b,x];
false.

Here are some other behaviors to explore ...
*******************************************************************/

:- write("insert(X,[a,b],[a,b,x])."),nl,forall(insert(X,[a,b],[a,x,b]), (write("X = "),write(X),write(";"),nl)),nl.
:- write("insert(X,Y,[a,b,c])."),nl,
      forall(insert(X,Y,[a,b,c]), (write("X = "),write(X),nl,write("Y = "),write(Y),write(";"),nl)),nl.
:- write("insert(x,Y,[a,b,x,c])."),nl,forall(insert(x,Y,[a,b,x,c]), (write("Y = "),write(Y),write(";"),nl)),nl.

/******************************************************************

This is a rather powerful aspect of prolog as it means that one predicate
can serve many purposes.  Here we can use the predicate to put an element
somewhere in the list OR (for examle) to remove an element from a list --
although that was not, perhaps, our original intention!

Let us consider evaluating
           insert(1,[2,3],Z).
first we note that there is a free variable Z in this predicate.  The evaluation
has the effect of instantiating the variable and what we have to show is how
that variable gets instantiated ...

Let us lable the clauses of insert:

(1) insert(X,Y,[X|Y]).
(2) insert(X,[H|Y],[H|Z]) :- insert(X,Y,Z).

Now whenever we pick up a clause we will do so with completely new variables
then we have:

goal:  insert(1,[2,3],Z).
          unify with (1) insert(X1,Y1,[X1|Y1]) success {1/X1,[2,3]/Y1,[1,2,3]/Z}
	       body empty: answer        ** Z=[1,2,3] ** ; (force backtracking)
	  unify with (2) insert(X2,[H2|Y2],[H2|Z2]) success {1/X2,2/H2,[3]/Y2,[2|Z2]/Z}
	        body (2.1): insert(X2,Y2,Z2) {1/X2,2/H2,[3]/Y2,[2|Z2]/Z}
		 = insert(1,[3],Z2)
		     unify with (1) insert(X3,Y3,[X3|Y3])
				  success {1/X3,[3]/Y3,[1,3]/Z2}
	               body empty: answer Z2=[1,3],         **Z= [2,1,3]** ;  (force backtracking)
		     unify with (2) insert(X4,[H4|Y4],[H4|Z4])
				   success {1/X4,3/H4,[]/Y4,[3|Z4]/Z2}
		        body (2.1): insert(X4,Y4,Z4){1/X4,3/H4,[]/Y4,[3|Z4]/Z2}
	                = insert(1,[],Z4)
			   unify with (1) insert(X5,Y5,[X5|Y5])
				       success {1/X5,[]/Y5,[1]/Z4}
		           body empty: answer Z4=[1],Z2=[3,1],         **Z=[2,3,1]**; (force backtracking)
			   unify with (2) insert((X6,[H6|Y6],[H6|Z6])
			                fail!!

There is quite alot happening but it is not completely unlike the Haskell hand evaluation: here
we use unification and we are non-deterministic in the sense that one we get a match we may
backtrack to find another match.

*******************************************************************/
	  