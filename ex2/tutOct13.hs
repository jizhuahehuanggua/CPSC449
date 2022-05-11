f :: Integer -> Integer -> a 

induction will only about list 
    -there are two way to store the value of a tree diagram 
        -either store in a list 
        -or store in the internal node 
        -make sure to know each



answer is not important as long as you makes sense 
 

(Tip = Leaf)
 binary  Tree: data tree a = Tip | Node a : (Tip a ) (Tip a)

depth Tip = 0                                                   1
depth (Node a t1 t2) = 1 + max (depth t1) (depth t2)

size Tip = 0                                                    2
size (Node a t1 t2) = 1 + size t1 + size t2

Prove: size t < 2

basecase:
    size tip < 2 (depth Tip)
    LHS = 0
    RHS = 2^0



    

