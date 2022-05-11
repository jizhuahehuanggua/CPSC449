profile (Rs x []) = SS [0]
profile rose = foldRose helper rose
    where
        uniform :: Eq a => [a] -> Bool
        uniform [] = True
        uniform [_] = True
        uniform (x:y:xs) = (x == y) && uniform (y:xs)

        getElem (SS (x:xs)) = x:xs

        getElems ((x:xs)) = getElem x

        helper :: a -> [SF [Int]] -> [Int]
        helper _ [] = SS [0]
        helper rose child
            | uniform child = SS (length child : getElems child)
            | otherwise = FF