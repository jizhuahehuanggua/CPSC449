-- foldr:: (a->b->b)->b->[a]->b
-- foldr f b [] = b
-- foldr f b (x:xs) = f x (foldr f b xs)

-- mord:: Ord a => a -> [a] -> Bool
-- mord x = foldr (\a b -> (a==x) || ((x>=a) && b)) False 


-- (||) :: Bool->Bool->Bool
-- (||) False  b = b
-- (||) _  _ = True

-- (&&) :: Bool->Bool->Bool
-- (&&) True b = b
-- (&&) _  _ = False

-- mord 2 [1,4,8,11,22] ？
-- mord 2 [1,4,8,11,22] :=  mord x [1,4,8,11,22]  ...match [ x := 2 ]
--     = foldr (\a b -> (a==x) || ((x>=a) && b)) False [1,4,8,11,22] [ x:= 2 ]
--     = foldr (\a b -> (a==2) || ((2>=a) && b )) False [1,4,8,11,22]

--     = foldr (\a b -> (a==2) || ((2>=a) && b )) False [1,4,8,11,22] ?
--     = foldr f b [] = b ... match fail
--     = foldr f b (x:xs) = f x (foldr f b xs) ...match [ f := (\a b -> (a==2) || ((2>=a) && b )), b := False, x := 1, xs := [4,8,11,22]]
--                        = f x (foldr f b xs) [f := (\a b -> (a==2) || ((2>=a) && b )), b := False, x:= 1,  xs := [4,8,11,22]]   
--                         = (\a b -> (a==2) || ((2>=a) && b )) 1 (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22])     --substitude anonymous function
--                                 = (1 == 2) || ((2>=1) && (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22])) ?
--                                 = (1 == 2) || ((2>=1) && (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22])) =: (||) False b 
--                                     | 1 == 2
--                                     | False  
--                                             = b [b := (2>=1) && (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22])] = (&&) True b 
--                                                              | 2>=1
--                                                              | True 
--                                                             = b [b := (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22]) ]
--                                                                 = (foldr (\a b -> (a==2) || ((2>=a) && b )) False [4,8,11,22]) ?
--                                                                 = foldr f b [] = b ...match fail
--                                                                 = foldr f b (x:xs) ...match [ f := (\a b -> (a==2) || ((2>=a) && b )), b := False, x := 4, xs := [8,11,22]]
--                                                                                     = f x (foldr f b xs) [ f := (\a b -> (a==2) || ((2>=a) && b )), b := False, x := 4, xs := [8,11,22]]
--                                                                                     = (\a b -> (a==2) || ((2>=a) && b )) 4 (foldr (\a b -> (a==2) || ((2>=a) && b )) False [8,11,22]) 
--                                                                                             = (4==2) || ((2>=4) && b )) 4 (foldr (\a b -> (a==2) || ((2>=a) && b ) False [8,11,22] ? 
--                                                                                             = (4==2) || ((2>=4) && b )) 4 (foldr (\a b -> (a==2) || ((2>=a) && b ) False [8,11,22] = (||) False b 
--                                                                                                      | 4 == 2
--                                                                                                      | False 
--                                                                                                         = b [b := (2>=4) && (foldr (\a b -> (a==2) || ((2>=a) && b )) False [8,11,22])] = (&&) True b 
--                                                                                                                                                                 | 2>=4
--                                                                                                                                                                 | False ...match fail
--                                                                                                                                                                 =: (&&) _ _ ...match []
--                                                                                                                                                                 = False 
--                                                                                                         = False 
--                                                                                     = False 
--                                                                 = False
--                                             = False 
--                         = False 
--     = False 





-- enum :: Int -> [Int]
-- enum n = n : enum (n+1)

-- myzip [] _ = []
-- myzip _ [] = []
-- myzip (a:as) (b:bs) = (a,b): (myzip as bs)

-- myfold :: (a -> b -> b) -> b -> [a] -> b
-- myfold f b [] = b
-- myfold f b (a:as) = f a (myfold f b as)

-- grow xs = myfold f [] (myzip (enum 1) xs)


-- myzip (enum 17) [1,2,3] ?

--         enum 17 ?
--         enum 17 := enum n ...match [n := 17]
--                 = n : enum (n + 1)  [n := 17]
--                 = 17 : enum (17 + 1)
--                 = 17 : enum 18
--                      = enum 18 ?
--                        enum 18 := enum n ...match [ n := 18]
--                                 = n : enum (n + 1)  [n := 18] 
--                                 = 18 : enum 19
--                 = [17,18,19...]
-- myzip [17 ,18, 19...] [1,2,3] := myzip [] _ ...match fail
-- myzip [17 ,18, 19...] [1,2,3] := myzip _ [] ...match fail
-- myzip [17 ,18, 19...] [1,2,3] := myzip (a:as) (b:bs) ...match [a := 17, as := [18,19...], b := 1, ba := [2,3]]
--                                 = (a,b): (myzip as bs) [a := 17, as := [18,19...], b := 1, ba := [2,3]]
--                                 = (17,1) : (myzip [18,19..] [2,3])
--                                           = myzip [18, 19...] [2,3] := myzip [] _ ...match fail
--                                           = myzip [18, 19...] [2,3] := myzip _ [] ...match fail
--                                           = myzip [18, 19...] [2,3] := myzip (a:as) (b:bs) ... match [a := 18, as := [19...], b := 2, bs := [3]]
--                                           = (a,b) : (myzip as bs) [a := 18, as := [19,20...], b := 2, bs := [3]]
--                                           = (18,2) : (myzip [19,20...] [3])
--                                                     = myzip [19,20...] [3] := myzip [] _ ...match fail
--                                                     = myzip [19,20...] [3] := myzip _ [] ...match fail
--                                                     = myzip [19,20...] [3] := myzip (a:as) (b:bs) ..match [a := 19, as := [20,21...], b := 3, bs := []]
--                                                     = (a,b) : (myzip as bs) [a := 19, as := [20,21...], b := 3, bs := []]
--                                                     = (19,3) : (myzip [20,21...] [])
--                                                             = myzip [20,21...] [] := myzip [] _ ...match fail
--                                                             = myzip [20,21...] [] := myzip _ [] ...match []
--                                                             = []
--                                                     = (19,3) : []
--                                                     = [(19,3)]
--                                           = (18,2) : [(19,3)]
--                                           = [(18,2),(19,3)]
--                                 = (17,1) : [(18,2),(19,3)]
--                                 = [(17,1),(18,2),(19,3)]

                                                        

-- -- enum :: Int -> [Int]
-- -- enum n = n : enum (n+1)

-- -- myzip :: [(Int, Char)] -> [Char] -> [((Int, Char), Char)]
-- -- myzip [] _ = []
-- -- myzip _ [] = []
-- -- myzip (a:as) (b:bs) = (a,b): (myzip as bs)

-- -- myfold :: ((Int, Char) -> Char -> Char) -> Char -> [(Int, Char)] -> Char
-- -- myfold f b [] = b
-- -- myfold f b (a:as) = f a (myfold f b as)

-- -- grow :: [Char] -> [(Int, Char)]
-- -- grow xs = myfold f [] (myzip (enum 1) xs)

-- -- f :: [Char] -> [(Int, Char)] -> [Char]
-- -- f xs ((num,c):rest)
-- --     | num == 0 = f xs rest
-- --     | otherwise = f xs ((num-1,c):rest)



-- -- pops :: [[Int]] -> [(Int, Int)]
-- -- pops xss = [ (x,length xs) | xs <- xss, x <- xs, x <= length xs]

-- -- new :: [[Int]] -> [(Int, Int)]
-- -- new xss = concat(map (\x -> {[(x,length xs) | x <- xs, x <= length xs ]}) xss   )

-- -- new2 xss = concat(map (\x -> { [(x,length xs) | x <- xs, x <= length xs ] }) xss   )

-- --                                 concat(map(\y -> {[(x,length xs) | x <= length xs]}   )  xs )
-- --                                                 if (x <= length xs) then [(x,length xs) |] else


-- -- pops' :: [[Int]] -> [(Int, Int)]
-- -- pops' xss = concat(map (\xs -> (concat(map(\x -> (if (x <= length xs) then [(x,length xs)] else []) )  xs ))) xss ) 
-- --             --     = concat(map (\xs -> ([(x,length xs) | x <- xs, x <= length xs ])) xss   )       			rule 2
-- --               --    =concat(map(\x -> ([(x,length xs) | x <= length xs])   )  xs )			rule 2
-- --             --                                     		    =if (x <= length xs) then [(x,length xs)] else []		rule 3
-- --             --                                      		      		        = [(x,length xs)]		rule 1


-- enum :: Int -> [Int]
-- enum n = n : enum (n+1)

-- myzip [] _ = []
-- myzip _ [] = []
-- myzip (a:as) (b:bs) = (a,b): (myzip as bs)

-- myfold :: (a -> b -> b) -> b -> [a] -> b
-- myfold f b [] = b
-- myfold f b (a:as) = f a (myfold f b as)

-- grow xs = myfold f [] (myzip (enum 1) xs)               

-- f :: ((Int, Char) -> [Char] -> [Char])  
-- f (num,c) xs
--     | num == 0 = xs 
--     | otherwise = f (num-1,c) (c:xs)

-- -- f (0,_) as = as
-- -- f (i,c) as = f (i-1,c) (c:as)

-- new2 xss = concat(map (\xs -> (  concat(map(\x -> (if (x <= length xs) then [(x,length xs) ] else [])   )  xs ) )) xss   )    


-- pops :: [[Int]] -> [(Int, Int)]
-- pops xss = [ (x,length xs) | xs <- xss, x <- xs, x <= length xs]

-- new2 xss = concat(map (\xs -> (  concat(map(\x -> (if (x <= length xs) then [(x,length xs) ] else [])   )  xs ) )) xss   )   

-- [[ [(x, y)| x <- as, y <- bs, f x == g y] ]]
-- concat(map(\(x, y) -> [[ [(x, y) | f x == g y ] ]]) as, bs)
-- concat(map(\x -> if (f x == g y) then [[ [(x,y) | ] ]] else []) as, bs)
-- concat(map(\x -> if (f x == g y) then [(x, y)] else []) as, bs)



-- data SF a = FF | SS a


-- data Rose a = Rs a [Rose a]

-- foldRose :: (a->[c]->c) ->(Rose a)->c
-- foldRose f (Rs a branches) = f a (map (foldRose f) branches)


-- enum :: Int -> [Int]
-- enum n = n : enum (n+1)

-- myzip [] _ = []
-- myzip _ [] = []
-- myzip (a:as) (b:bs) = (a,b): (myzip as bs)

-- myfold :: (a -> b -> b) -> b -> [a] -> b
-- myfold f b [] = b
-- myfold f b (a:as) = f a (myfold f b as)

-- grow xs = myfold f [] (myzip (enum 1) xs)

-- myzip (enum 17) [1,2,3] ?
-- myzip [] _ ...match fail
-- myzip _ [] ...match fail
-- myzip (a:as) (b:bs) =: enum 17
--         | enum 17 
--         = enum n ...match [n := 17]
--         = n : enum (n + 1)
--         = 17 : enum (18) 
--         ...match [a := 17, as := enum 18, b := 1, bs := [2,3]]
--         = (a,b): (myzip as bs)
--         = (17,1) : (myzip (enum 18) [2,3])
--                  = myzip (enum 18) [2,3]
--                  = myzip [] _ ...match fail
--                  = myzip _ [] ...match fail
--                  = myzip (a:as) (b:bs) =: enum 18
--                             | enum 18 
--                             = enum n ...match [n := 18]
--                             = n : enum (n + 1)
--                             = 18 : enum (19) 
--                             ...match [a := 18, as := enum 19, b := 2, bs := [3]]
--                             = (a,b): (myzip as bs)
--                             = (18, 2) : (myzip (enum 19) [3])
--                                         = myzip (enum 19) [3]
--                                         = myzip [] _ ...match fail
--                                         = myzip _ [] ...match fail
--                                         = myzip (a:as) (b:bs) =: enum 19
--                                                    | enum 19
--                                                    = enum n ...match [n := 19]
--                                                    = n : enum (n + 1)
--                                                    = 19 : enum (20) 
--                                                    ...match [a := 19, as := enum 20, b := 3, bs := []]
--                                                    = (a,b): (myzip as bs)
--                                                    = (19, 3) : (myzip (enum 20) [])
--                                                                 = myzip [] _ ...match fail
--                                                                 = myzip _ [] ...match []
--                                                                 = []
--                                                     = (19,3) : []
--                                                     = [(19,3)]
--                             = (18,2): [(19,3)]
--                             =  [(18,2),(19,3)]
--         = (17,1) : [(18,2),(19,3)]
--         = [(17,1),(18,2),(19,3)]




data Rose a = Rs a [Rose a]

data SF a = FF | SS a

dfsRSTree :: Rose a -> [a]
dfsRSTree (Rs a rs) = a : go rs --xs is the list of nodes returned by rs
    where
        go [] = []
        go (rs:rss) = (dfsRSTree rs) ++ (go rss)



foldRose :: (a->[c]->c) ->(Rose a)->c
foldRose f (Rs a branches) = f a (map (foldRose f) branches)

all_heights1 :: Rose a -> [Int]
all_heights1 = foldRose f  -- (RS a rs)
    where
        f a [] = [1]
        f a xs = map (1+) (concat xs) -- xs :: [[Int]]

profile :: Rose a -> SF [Int]
profile (Rs a []) = SS [0]
profile r = profile' r

isUniform :: Rose a -> Bool
isUniform (x:xs:xss)
    | x == xs = isUniform (xs:xss)
    | otherwise = False



mytree = Rs 1 [Rs 2[Rs 4 [Rs 7 []], Rs 5 [], Rs 6 []], Rs 3[]]

tree = Rs 1 [Rs 2 [], Rs 3 [Rs 4 [], Rs 5 [], Rs 6 []]]