fac :: Integral a => a -> a
fac 1 = 1
fac n = n * fac (n-1)

fac1 :: Integral a => a -> a
fac1 n = if n == 1
    then 1
    else n * fac1 (n-1)

fac2 :: Integral a => a -> a
fac2 n = case n of
    1 -> 1
    _ -> n * fac2 (n-1)

fac3 :: Integral a => a -> a
fac3 n
    | n < 1 = error "Error: n must be greater than 0!"
    | n == 1 = 1
    | otherwise = n * fac3 (n-1)

-- hypotenuse formula
foo a b = sqrt(a^2 + b^2)
foo1 a b = sqrt(x + y)
    where 
        x = a^2
        y = b^2
foo2 a b = 
    let x = a^2; y = b^2
    in sqrt(x + y)


-- Functions for checking whether a list belongs to the Fibonacci sequence.

-- n is a Fib num if 5n^2+4 or 5n^2-4 is a perfect square.
isPerfectSqr :: Int -> Bool
isPerfectSqr n = x == fromIntegral (round x) where
    x = sqrt (fromIntegral n)

isFibonacciNum :: Int -> Bool
isFibonacciNum n = isPerfectSqr(5 * n^2 + 4) || isPerfectSqr(5*n^2 - 4)

-- Recursively check whether Fn = Fn-1 + Fn-2
isFib :: [Int] -> Bool
isFib (a:b:c:xs)
    | xs == [] = if a + b == c then True else False
    | otherwise = (a+b == c) && isFib (b:c:xs)

{- 
First, check whether the list is longer than 3.
Then, check whether the first two elements are Fib num. 
-}
isFibonacciSeq :: [Int] -> Bool
isFibonacciSeq xs 
    | length xs < 3 = False 
    | otherwise = isFibonacciNum a && isFibonacciNum b && isFib xs
        where a:b:_ = xs

-- Create our own Prelude functions
length' xs = sum [1 | _ <- xs]

fst' (a, _) = a 
snd' (_, b) = b

head' :: [a] -> a
head' [] = error "Empty list."
head' (x:xs) = x 

last' :: [a] -> a
last' [] = error "Empty list."
last' [x] = x 
last' (_:xs) = last' xs 