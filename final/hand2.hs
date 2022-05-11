-- data STree a = Tip | Snode (STree a) a (Stree a)

-- intree n Tip = False
-- intree n (Snode t1 m t2) 
--   = (n == m) || (intree n t1 || intree n t2)
                               
-- False || b = b
-- _ || _ = True  

-- foldSTree f g Tip = f
-- foldSTree f g (Snode t1 a t2) 
--       = g (foldSTree f g t1) a (foldSTree f g t2)


-- intree 2 (Snode (Snode Tip 11 Tip) 2 (Snode Tip 7 Tip)) ?
-- intree 2 (Snode (Snode Tip 11 Tip) 2 (Snode Tip 7 Tip)) =: intree n Tip  ...match fail
-- intree 2 (Snode (Snode Tip 11 Tip) 2 (Snode Tip 7 Tip)) =: intree n (Snode t1 m t2)  ...match [n := 2, t1 := (Snode Tip 11 Tip), m := 2, t2 := (Snode Tip 7 Tip)]
--     = (n == m) || (intree n t1 || intree n t2)
--     = (2 == 2) || (intree 2 (Snode Tip 11 Tip) || intree 2 (Snode Tip 7 Tip)) ?
--     = (2 == 2) || (intree 2 (Snode Tip 11 Tip) || intree 2 (Snode Tip 7 Tip)) =: False || b  
--         | 2 == 2
--         | True  ...match fail
--     = (2 == 2) || (intree 2 (Snode Tip 11 Tip) || intree 2 (Snode Tip 7 Tip)) =: _ || _   ...match
--     = True
    
        
    
data Tree a = Leaf a
            | Node (Tree a) (Tree a)

mapTree :: (t -> a) -> Tree t -> Tree a
mapTree f  (Leaf a) = Leaf (f a)
mapTree f (Node t1 t2) 
       = Node (mapTree f t1) (mapTree f t2)

foldTree :: (t1 -> t2) -> (t2 -> t2 -> t2) -> Tree t1 -> t2
foldTree leaf node (Leaf a) = leaf a
foldTree leaf node (Node t1 t2) 
       = node (foldTree leaf node t1) (foldTree leaf node t2)

collect :: Tree a -> [a]
collect = foldTree (\a -> [a]) (++)



intree :: Eq a => a -> STree a -> Bool 
intree x t = exist x (all' t)

help :: [a] -> a -> [a] -> [a]
help xs y zs = xs ++ [y] ++ zs

all' :: (STree a) -> [a] 
all' Tip = []
all' t  = foldSTree [] help t

exist ::(Eq a) => a -> [a] -> Bool
exist _ [] = False 
exist x (y:ys)
    |x == y = True 
    |otherwise = exist x ys


base_num :: Int -> [Int] -> (SF Int)
base_num m list = foldr func (SS 0) (calNum m (length(list)-1) list)
    
func :: Int -> (SF Int) -> (SF Int)
func x (SS y)
    |x == (-1) = FF
    |otherwise = SS (x+y)
func _ FF = FF

calNum :: Int -> Int -> [Int] -> [Int]
calNum m pow [] = []
calNum m pow (x:xs)
    | ((x < m) && (x >= 0)) = (x * m^pow) : (calNum m (pow-1) xs)
    | otherwise = (-1) : (calNum m (pow-1) xs)