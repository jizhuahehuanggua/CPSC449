



data Rose a = Rs a [Rose a]
  deriving (Show, Eq, Ord, Read)

data SF a = SS a | FF
  deriving (Show, Read, Eq, Ord)
dfsRSTree :: Rose a -> [a]
dfsRSTree (Rs a rs) = a : go rs --xs is the list of nodes returned by rs
    where
        go [] = []
        go (rs:rss) = (dfsRSTree rs) ++ (go rss)


trEE = Rs 1 [Rs 1[], Rs 1[]]

foldRose :: (a->[c]->c) ->(Rose a)->c
foldRose f (Rs a branches) = f a (map (foldRose f) branches)

profile :: Rose a -> SF [Int]
profile rs = foldRose func rs
    where
        func rs [] = SS [0]
        func rs re
            | isUniform re == True = SS(profile' re)    --if its uniform then SS + get the of number of nodes 
            | isUniform re == False = FF        --if its false then FF

        profile' re = [length re] ++ getList re     

getList :: [SF [Int]] -> [Int]
getList ((SS xs):xss) = xs

--to test if its a uniform
isUniform :: Eq a => [a] -> Bool
isUniform [] = True
isUniform [x] =  True
isUniform (x:xs:sss) 
    | x \= xs = False 
    | otherwise = isUniform (xs:sss) 



all_heights1 :: Rose a -> [Int]
all_heights1 = foldRose f  -- (RS a rs)
    where
        f a [] = [1]
        f a xs = map (1+) (concat xs) -- xs :: [[Int]]

-- profile :: Rose a -> [Int]
-- profile  = foldRose helper 
--     where
--         uniform :: Eq a => [a] -> Bool
--         uniform [] = True
--         uniform [_] = True
--         uniform (x:y:xs) = (x == y) && uniform (y:xs)

--         helper :: a -> [Int] -> [Int]
--         helper _ [] = [0]
--         helper rose child
--             | uniform child = length child : child
--             | otherwise = [0]

-- profile (Rs x []) = SS [0]
-- profile rose = foldRose helper rose
--     where
--         uniform :: Eq a => [a] -> Bool
--         uniform [] = True
--         uniform [_] = True
--         uniform (x:y:xs) = (x == y) && uniform (y:xs)

--         getElem (SS (x:xs)) = x:xs

--         getElems ((x:xs)) = getElem x

--         helper :: a -> [SF [Int]] -> [Int]
--         helper _ [] = SS [0]
--         helper rose child
--             | uniform child = SS (length child : getElems child)
--             | otherwise = FF



mytree = Rs 1 [Rs 2[Rs 4 [Rs 7 []], Rs 5 [], Rs 6 []], Rs 3[]]

tree = Rs 1 [Rs 2 [], Rs 3 [Rs 4 [], Rs 5 [], Rs 6 []]]

trEE = Rs 1 [Rs 1[], Rs 1[]]





myadd a b | trace ("print something") => show 

