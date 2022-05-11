--import Prelude( , Integer, Eq, Ord )

--import Prelude(Int, Float, Integer, Eq, Ord, Bool (..), Double, String, otherwise, Num, Integral, Fractional, abs, (+), (-), (^), subtract, (*), (/), signum, (==), (/=), (<), (<=), (>), (>=), compare, (||), (&&), not, rem, mod, div, quot, max, min, fromIntegral, toInteger, undefined, error, Show, show, Bounded, minBound, maxBound, seq)

--import Prelude

-- myconcat :: [a] ->  [a] ->  [a]             
-- myconcat [] x =  x
-- myconcat (x:xs) ys  = x : myconcat xs ys 

-- mfoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
-- mfoldr f g [] = g
-- mfoldr f g (x:xs) = f x (mfoldr f g xs)

-- mmap f [] = []
-- mmap f (x:xs) = (f x):(mmap f xs)

-- pain :: [a] -> [[a]]
-- pain =  mfoldr (\a s -> s ++ (mmap ((:) a) s)) [[]]

-- misery :: (Num a, Ord a) => a -> [a] -> [[a]]
-- misery m xs = [ys | ys <- pain xs, m >= (foldr (+) 0 ys)]


-- mfoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
-- mfoldr f g [] = g
-- mfoldr f g (x:xs) = f x (mfoldr f g xs)

-- data SF a = SS a | FF
--     deriving (Show, Eq, Ord, Read)

-- base_num :: Int -> [Int] -> (SF Int)
-- base_num m list = foldr func (SS 0) (calNum m (length(list)-1) list)
    
-- func :: Int -> (SF Int) -> (SF Int)
-- func x (SS y)
--     |x == (-1) = FF
--     |otherwise = SS (x+y)
-- func _ FF = FF

-- calNum :: Int -> Int -> [Int] -> [Int]
-- calNum m pow [] = []
-- calNum m pow (x:xs)
--     | ((x < m) && (x >= 0)) = (x * m^pow) : (calNum m (pow-1) xs)
--     | otherwise = (-1) : (calNum m (pow-1) xs)

-- data STree a = Tip | Snode (STree a) a (STree a)

-- mfoldSTree f c Tip = c
-- mfoldSTree f c (Snode t1 a t2) = f (mfoldSTree f c t1) a (mfoldSTree f c t2)

-- intree :: Eq t => t -> STree t -> Bool
-- intree n Tip = False
-- intree n (Snode t1 m t2) 
--   = (n == m) || (intree n t1 || intree n t2)
                               
-- False || b = b
-- _ || _ = True

-- foldSTree :: t1 -> (t1 -> t2 -> t1 -> t1) -> STree t2 -> t1
-- foldSTree f g Tip = f
-- foldSTree f g (Snode t1 a t2) 
--       = g (foldSTree f g t1) a (foldSTree f g t2)

-- intree :: Eq a => a -> STree a -> Bool 
-- intree n tr = foldSTree False (\t1 a t2 -> if (a==n) then True else (t1 || t2) ) tr


-- intree:: Eq a => a -> STree a -> Bool
-- intree n Tip = False
-- intree n (Snode t1 a t2) = foldSTree False (check5 n) (Snode t1 a t2)

-- check5::Eq a => a -> Bool -> a -> Bool -> Bool
-- check5 n flag1 a flag2 = (n==a) || (flag1 || flag2)

