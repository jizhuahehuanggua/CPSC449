/*
Prolog - logic program 
        

*/

/*insert().*/


mother(mary, amy).
mother(mary,lili).

sibling(A,B) :- mother(mary,A),mother(mary,B).

siblingT(A,B) :- mother(M, A), mother(M, B), A \= B, !.
siblingT1(A,B) :- father(F,A), father(F,B), A \= B.
/*sibling(A,B). 
 A = amy,
 B = lili;
 A = lili,
 B = amy;

prolog is back tracking

make.

sibling(A,B),
A = amy,
B = lili,






 */
fact5_x(1).
fact5_x(2).
fact5_y(3).
fact5_y(4).
fact5_z(5).
fact5_z(6).

solve1(X,Y,Z) :- fact_x(X), fact_y(Y), fact_(Z).
solve2(X,Y,Z) :- fact_x(X), fact_y(Y),! , fact_(Z).
solve3(X,Y,Z) :- fact_x(X),! , fact_y(Y) , fact_(Z).
/*↑whenever you find the unification for X, you keep going on for Y and Z
  but when you back tracking to here &^&^*$#*!@
*/

/*
solve1(X,Y,Z)
X = 1,
Y = 3,
Z = 5;
X = 1,
Y = 2,
Z = 6;
X = 1,
Y = 4,
Z = 5;


[X|YS] = [1,2,3,4].
X = 1
YS = [2,3,4]

mymember (X,[X| ]) :- !.
mymember(X, [ |YS]) :- mymember (X, YS).






infect('Adrian12598',21),infect('Bay12598',17)
[
met('Aspen12598',5,'Bay12598'),
met('Bobbie12598',19,'Brooklyn12598'),
met('Blake12598',9,'Corey12598'),
met('Corey12598',29,'Charlie12598'),
met('Blair12598',6,'Adrian12598'),
met('Dakota12598',17,'Aspen12598'),
met('Brooklyn12598',30,'Dakota12598'),
met('Bay12598',11,'Brooklyn12598'),
met('Cameron12598',4,'Ash12598'),
met('Dakota12598',27,'Charlie12598'),
met('Blake12598',19,'Amari12598'),
met('Amari12598',26,'Blake12598'),
met('Blake12598',20,'Adrian12598'),
met('Charlie12598',12,'Aspen12598'),
met('Carson12598',12,'Ariel12598'),
met('Amari12598',8,'Aspen12598'),
met('Amari12598',12,'Charlie12598'),
met('Bobbie12598',18,'Charlie12598'),
met('Adrian12598',16,'Dallas12598'),
met('Cameron12598',3,'Blair12598'),
met('Blake12598',11,'Blair12598'),
met('Bay12598',19,'Dallas12598'),
met('Blair12598',20,'Ariel12598'),
met('Aspen12598',15,'Adrian12598'),
met('Ariel12598',13,'Blair12598'),
met('Brooklyn12598',13,'Adrian12598'),
met('Adrian12598',20,'Corey12598'),
met('Bobbie12598',19,'Corey12598'),
met('Bay12598',18,'Bobbie12598')
]

[[infect('Bay12598',18),infect('Bobbie12598',19),infect('Corey12598',20),infect('Adrian12598',21)]]








*/