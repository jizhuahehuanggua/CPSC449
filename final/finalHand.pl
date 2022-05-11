/*
(1) remove(X,[X|Xs],Xs).
(2) remove(X,[H|Y],[H|Ys]):- 
    (2.1)remove(X,Y,Ys).

goal: remove(X,[a,b],Y).
    unify with (1) remove(X,[X|Xs],Xs). success {a/X1, [b]/Xs1, [b]/Y1}
        body empty : answer     **X = a, Y = [b]**; (force backtracking)
    unify with (2) remove(X,[H|Y],[H|Ys]) success {X/X2, a/H1, [b]/Y2, [a|Ys1]/Y2}
        body(2.1): remove(X,Y,Ys). {X/X2, [b]/Y2, [a|Ys1]/Y2}
                = remove(X,[b],Ys1)
                    unify with (1) remove(X,[X|Xs],Xs). success{b/X3, []/Xs2, []/Ys1}
                        body empty : answer     **X = b, Y = [a|[]] = [a]; (force backtracking)
                    unify with (2) remove(X,[H|Y],[H|Ys]). success {X/X4, b/H2, []/Y3, [b|Ys2]/Y3}
                        body(2.1): remove(X,Y,Ys). {X/X4, []/Y3, [b|Ys2]/Y3}
                            = remove (X,[],Ys2)
                                unify with (1) remove(X,[X|Xs],Xs). fail!
                                unify with (2) remove(X,[H|Y],[H|Ys]). fail!
                            False.


remove(X,[X|Xs],Xs):- !.
remove(X,[H|Y],[H|Ys]):- remove(X,Y,Ys).

seating(InvitationList,SeatingPlan):- ...

feud_eruption(SeatingPlan) :- feud_eruption_SOS(SeatingPlan).

feud_eruption_SOS([X]).
feud_eruption_SOS([X,Y|XS]) :- (feud(X,Y)|feud(Y,X)), feud_eruption_SOS([Y|XS]).

feud(macbeth,duncan).
feud(macbeth,macduff).
feud(macdonald,cameron).
feud(alpin,bruce).
feud(macdougal,macduff).
*/


data STree a = Tip | Snode (STree a) a (Stree a)

intree n Tip = False
intree n (Snode t1 m t2) 
  = (n == m) || (intree n t1 || intree n t2)
                               
False || b = b
_ || _ = True  

foldSTree f g Tip = f
foldSTree f g (Snode t1 a t2) 
      = g (foldSTree f g t1) a (foldSTree f g t2)

