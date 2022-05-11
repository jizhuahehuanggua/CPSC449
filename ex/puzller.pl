% -*- Mode:Prolog -*-

/*
                        Solving puzzles.

  These notes show a general "template" for soving puzzles due to Sterling
  and Shapiro in the "Art of Prolog".

  This is definitely not the only way to solve puzzles but it demonstates 
  an interesting use of prolog!!

  The idea is to set up a puzzle in a  general manner so that the structure 
  of the puzzle, its clues, and  the queries which must be solved are all 
  specified in a general way.

  The general code is:
*/

:- discontiguous structure/2,queries/4,clues/3.

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

/*
   To this general code must be added the specifics of a puzzle.  Here is a 
   typical puzzle:

   Three friends came first, second, and third in a checkers ai play-off.  
They all have different first names, liked different sports, and came from 
a different Canadian city.

  Mike likes ice hockey, and did better than the Edmontonian. Simon who was 
from Vancouver, did better than the rower. However, the soccer player won the 
competition.

  What sport does Julie play? Who is from Calgary? Who won the competition?

  We represent a friend by the structure:

     friend(Name,City,Sport).

*/


structure(friends,[friend(_,_,_),friend(_,_,_),friend(_,_,_)]).
%  How many possibilities are there?
%  There are (3!)^3 = 216 possibilities!  Just about searchable by hand!

clues(friends,Friends,
   % the clues
      [(did_better(P1Clue1,P2Clue1,Friends),
       name(P1Clue1,mike),sport(P1Clue1,icehockey),
       city(P2Clue1,edmonton))
   % Mike who plays icehockey did better than the Edmontonian
      ,(did_better(Man1Clue2,Man2Clue2,Friends),
       name(Man1Clue2,simon),city(Man1Clue2,vancouver),
       sport(Man2Clue2,rowing))
   % Simon who is from Vancouver did better than the rower
      ,(first(Friends,ManClue3),sport(ManClue3,soccer))
   % The soccer player won
      ]).

queries(friends,Friends,
   % the queries
        [ member(Q1,Friends),
          name(Q1,Name),
          city(Q1,calgary),
   % who (Name) lives in Calgary
          member(Q2,Friends),
          name(Q2,julie),
          sport(Q2,Sport),
   % What Sport does julie play
	  first(Friends,First),
	  name(First,FName)
   % Who (FName) came first
        ],
   % The solution
         [['The Calgarian is ',Name],
          ['Julie does ',Sport],
	  ['The winner was ',FName]]).

% Helpful predicates!!
did_better(A,B,[A,B,_]).
did_better(B,C,[_,B,C]).
did_better(A,C,[A,_,C]).

name(friend(A,_,_),A).
city(friend(_,B,_),B).
sport(friend(_,_,C),C).

first([X|_],X).

/*
   Now try:

            solver(friends,S).

   Now how did that happen!!
*/

/* Here is another puzzle (I got this from Niran Pon):

   Four boys are sitting on a couch: they each have distinct names, are eating different snacks, 
   have different shirt colours, prefer different genres of movies, and are different ages.


Clues:
   Joshua is sitting at one of the ends of the couch.
   The boy wearing a black shirt is to the left of the boy aged 10.
   Joshua likes horror movies.
   The 14-year-old boy is third from the left.
   The boy wearing the red shirt is to the right of the 13-year-old boy and to the left of the one who likes action movies.
   Daniel likes thriller movies.
   The boy who is eating cookies is at one of the ends of the couch.
   The boy wearing the black shirt is immediately to the left of the one who likes thriller movies.
   The boy who is eating crackers is immediately to the right of the boy who likes comedy movies.
   The boy wearing the red shirt is to the right of boy who is eating popcorn and to the left of Nicholas.
   At one of the ends is the boy who likes thriller movies.
   Nicholas is to the right of Joshua and to the left of Daniel.
   One of the boys is Simon who is 12 and is eating chocolate.
   At the left end of the couch is the boy wearing the Green shirt.

Queries:
   What genre of movies does the boy with a white shirt like?
   What color is Simon's shirt?
   How old is Daniel?

*/

structure(couch_potatoes,[couch(_,_,_,_,_),couch(_,_,_,_,_),couch(_,_,_,_,_),couch(_,_,_,_,_)]).

%  How many possibilities are there?
%  Answer (4!)^5 = (24)^5 = 7962624  ... this is a big number to search by hand!!

clues(couch_potatoes,Couch,
      % clues
      % Joshua is at one of the ends.
       [ (couch_ends(P11,P12,Couch),(couch_name(P11,joshua);couch_name(P12,joshua))),
      % The boy wearing the Black shirt is somewhere to the left of the boy aged 10.
         (couch_to_left(P21,P22,Couch), couch_shirtColor(P21,black), couch_age(P22,10)),
      % Joshua likes Horror movies.
         (member(P30,Couch), couch_name(P30,joshua), couch_genre(P30,horror)),
      % The 14-year-old boy is third from the left.
         (Couch=[_,_,P40|_],couch_age(P40, 14)),
	 % The boy wearing the red shirt is somewhere to the right of the 13-year-old boy
	 % and to the left of the one who likes Action movies.
         (couch_to_left(P51,P52,Couch),couch_to_left(P52,P53,Couch), couch_shirtColor(P52,red),
	  couch_age(P51,13), couch_genre(P53,action)),
      % Daniel likes Thriller movies.
         (member(P60,Couch), couch_name(P60, daniel), couch_genre(P60, thriller)),
      % The boy who is eating cookies is at one of the ends.
         (couch_ends(P71,P72,Couch), (couch_snack(P71,cookies);couch_snack(P72,cookies))),
      % The boy wearing the Black shirt is immediately to the left of the one who likes Thriller movies.
         (couch_neighbor(P81,P82,Couch), couch_shirtColor(P81,black), couch_genre(P82,thriller)),
      % The boy who is eating crackers is immediately to the right of the boy who likes comedy movies.
         (couch_neighbor(P91,P92,Couch), couch_genre(P91,comedy), couch_snack(P92,crackers)),
      % The boy wearing the Red shirt is to the right of boy who is eating popcorn and to the left of Nicholas.
         (couch_to_left(P101,P102,Couch), couch_to_left(P102,P103,Couch), couch_shirtColor(P102,red),
	  couch_snack(P101,popcorn), couch_name(P103,nicholas)),
      % At one of the ends is the boy who likes thriller movies.
         (couch_ends(P111,P112,Couch),(couch_genre(P111, thriller);couch_genre(P112,thriller))),
      % Nicholas is to the right of Joshua and to the left of Daniel.
         (couch_to_left(P121,P122,Couch),couch_to_left(P122,P123,Couch),couch_name(P121,joshua),
	  couch_name(P122,nicholas),couch_name(P123,daniel)),
      %  One of the boys is Simon who is 12 and is eating chocolate
	 (member(P150,Couch),couch_name(P150,simon),couch_snack(P150,chocolate),couch_age(P150,12)),
      % At the left end of the couch is the boy wearing the Green shirt.
         (Couch=[P160|_], couch_shirtColor(P160,green))]).
	 % Queries
queries(couch_potatoes,Couch,
          % What genre does the boy with a white shirt like?
          [(member(Q1,Couch),couch_shirtColor(Q1,white), couch_genre(Q1,Genre)),
	  % What shirt color does Simon wear?
	  (member(Q2,Couch),couch_name(Q2,simon),couch_shirtColor(Q2,Color)),
	  % How old is daniel?
	  (member(Q3,Couch),couch_name(Q3,daniel),couch_age(Q3,Age))],
	  [['The boy with the white shirt likes',Genre,'movies!']
	   ,['Simon wears a',Color,'shirt!']
	   ,['Daniel is',Age]]).
	   


% Useful predicates ....
couch_name(couch(Name, _, _, _, _),Name).
couch_age(couch(_,Age, _, _, _),Age).
couch_shirtColor(couch(_, _,ShirtColor, _, _),ShirtColor).
couch_genre(couch(_, _, _,Genre, _),Genre).
couch_snack(couch(_, _, _, _,Snack),Snack).

couch_ends(PFirst,PLast,[PFirst,_,_,PLast]).

couch_neighbor(P1, P2,[P1,P2|_]).
couch_neighbor(P1, P2, [_ | Rest]) :- couch_neighbor(P1, P2, Rest).

couch_to_left(P1,P2,[P1|Rest]):- member(P2, Rest).
couch_to_left(P1,P2,[_| Rest]):- couch_to_left(P1,P2,Rest).

/*
   Now try:

            solver(couch_potatoes,S).

   Now how did that happen!!
*/