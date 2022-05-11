foldRose :: (a->[c]->c) ->(Rose a)->c
foldRose f (Rs a branches) = f a (map (foldRose f) branches)

isUniform :: Eq a => [a] -> Bool
isUniform [] = True
isUniform [x] =  True
isUniform (x:xs:sss) 
    | x == xs = isUniform (xs:sss)
    | otherwise = False

profile :: Rose a -> SF [Int]
profile rose = foldRose func rose
    where
        func _ [] = SS [0]
        func rs re
            | isUniform re == True =  SS(profile' re)
            | otherwise = FF

        profile' re = length re : getList re

        getList :: [SF [Int]] -> [Int]
        getList ((SS xs):xss) = xs