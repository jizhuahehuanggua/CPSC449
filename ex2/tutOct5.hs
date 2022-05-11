{--
stuctural inducrion
    proof the data type and

    f(k) , f(k+1) 
    x， (x:xs)

we want to show:
    sum(xs ++ ys) = sum xs + sum ys 

proof:
    we need to prove two goals:
        1. for base case, we have to prove sum ([] ++ ys) = sum[] + sum ys (base)
        2. for induction step, we have to prove sum ((x:xs) ++ ys) = sum (x:xs) ++ sum ys (ind)
            on the assumption that sum(xs ++ ys) = sum xs + sum ys (hyp)

Base:
    
LHR : sum([] ++ ys)
      = sum ys              by ++1 
      

RHS : sum[] + sum ys 
      = sum ys              by s1
    
LHS = RHS.

induction:

LHS : sum((x:xs) ++ ys)
      = sum (x : (xs ++ ys))    by ++2
      = x + sum (xs ++ ys)      by s2 
      = x + sum xs + sum ys     by hyp 

RHS : sum (x:xs) ++ sum ys
      = x + sum xs + sum ys     by s2 

LHS = RHS. 



we want to show:
xs ++ [] = xs

proof:
    we need to prove two goals:
        1. for base case, we have to prove [] ++ [] = [] (base)
        2. for induction step, we have to prove sum (x:xs) ++ [] = (x:xs) (ind)
            on the assumption that sum(xs ++ ys) = sum xs + sum ys (hyp)




all_path:: Rose a -> [[a]]
all_path = foldRoss f 
    where 
        f a [] = [[a]]
        f a xs = map (a:) (concat xs)

all_height :: ROse a -> [Int]           --return the length of each leaf
all_height = foldRose f 
    where
        f a [] = [1]
        f a xs = map(1+) (concat xs)

min_height :: Rose a ->Int
min_height = foldRose f 
    where
        f a [] = 1
        f a xs = 1 + minimum xs              --xs is a list of  minimum heights for each child


all_leaf :: Rose a -> [a]
all_leaf = foldRose f 
    where 
        f a [] = [a] 
        f a xs = concat xs


sum_leaf :: (Rose a ) -> Rose a -> a
sum_leaf = foldRose f 
    where 
        f a [] = a 
        f a xs = sum xs
data Tree a = Nil | Node a (Tree a) (Tree a)
        deriving (Eq,Ord,Show,Read)

height :: (Ord a, Num a) => Tree a -> a
height Nil = -1
height (Node k l r) = 1 + (max (height l) (height r))

balanced :: (Ord a, Num a) => Tree a -> Bool
balanced Nil = True
balanced  (Node k l r) | not (balanced l) = False
                       | not (balanced r) = False
                       | abs ((height l) - (height r)) > 1 = False
                       | otherwise = True

--}

data Rose a = RS a [Rose a]
  deriving (Show, Eq, Ord, Read)



mytree = RS 1 [RS 2[RS 4 [RS 7 []], RS 5 [], RS 6 []], RS 3[]]


-- find all paths from root to leaf
{- Recursive step : assume we know the "all path" of the children,
we just need to append current node to those paths
-}
all_paths :: Rose a -> [[a]]
all_paths (RS a rs) = go a rs
    where
        go a [] = [[a]]  -- returns a list of paths of a list of tose trees
        go a rs = map (a:) (concat (map all_paths rs)) -- [[a]]

data STree a
    = Node (STree a) a (STree a)
    | Leaf
  deriving (Show, Eq, Ord, Read)


LessNode :: STree a -> STree a -> Bool
LessNode (Node t1 v1 t2) (Node t3 v2 t4) | v1 < v2 = True
                                         | otherwise = False
LessNode _ _ = False

-- t1 = Node Leaf 3 Leaf
-- t2 = Node (Node Leaf 3 Leaf) 5 (Node Leaf 4 Leaf)

main = do 
    print ( LessNode (Node Leaf 3 Leaf) (Node (Node Leaf 3 Leaf) 5 (Node Leaf 4 Leaf)))


