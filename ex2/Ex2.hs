-- | Exercise 2 template.
-- Instructions. 
--      1. You are to write your solutions to the exercise in this file. You
--      may add extra functions in this file to implement your solution.
--
--      2. You are to comment your code indicating how you arrived at a
--      solution.
--
--      3. It is best to avoid Prelude and library functions not already
--      imported and develop your own code for your solutions.
--
--      4. Do NOT change the type or name of any of the template functions. 
--
--      5. Do NOT change the name of this file. 
--
--      6. Do NOT change the module name of this file.
--
--      7. To submit this to GradeScope, submit ONLY this file.
--
--      8. Have lots of fun :). Wahoo!!
module Ex2 where

-- This imports the required types and support functions for the questions.
import Ex2Types

-- This imports some standard Prelude functions for you to use.
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), Double, String, otherwise, Num, Integral, Fractional, abs, (+), (-), (^), subtract, (*), (/), signum, (==), (/=), (<), (<=), (>), (>=), compare, (||), (&&), not, rem, mod, div, quot, max, min, fromIntegral, toInteger, undefined, error, Show, show, Bounded, minBound, maxBound, seq)
import Debug.Trace

-- | Q1.
-- if four kinds of different possible input are returning true, then it is totaulogy
twoTautology :: ((Bool, Bool) -> Bool) -> Bool
twoTautology f  = f (True, True) && f (True, False) && f (False, True) && f (False, False)


--compare two functions four kind of input, if their output are all true then it is equivalent, otherwise return it is not 
twoEquiv :: ((Bool, Bool) -> Bool) -> ((Bool, Bool) -> Bool) -> Bool
twoEquiv f g = (f (True, True) == g (True, True) ) && (f (True, False) == g (True, False))&& (f (False, True) == g (False, True))&& (f (False, False) == g (False, False))

-- Q2 algorithm: creat a list of integer from 1 to infinite,
--               test integer one by one to see if its prime in form of 2^2^n


-- | Q2
badFermat :: Integer
badFermat = findNonPrime [2^(2^x)+1 |x <- [0,1..] ]     --pass a list of integer in the form of 2^2^x from x = 0
--checkPrime: get a list of integers, return the first non prime number it meets   
findNonPrime :: [Integer] -> Integer
findNonPrime (y:ys)  
    | [ z | z <- [2,3..y-1], y `mod` z == 0] /= [] = y 
    | otherwise = findNonPrime ys

--merge two list into one list
concat :: [a] ->  [a] ->  [a]             
concat [] x =  x
concat (x:xs) ys  = x : concat xs ys 

-- | Q3
collatzIndex :: Int -> SF [Int]     -- if x <= 0 is FF, if x > 0 calculate the list 
collatzIndex x
    |x <= 0 = FF 
    |otherwise = SS (myCollatzIndex x)

myCollatzIndex :: Int -> [Int]
myCollatzIndex num
    |num == 1 = [num]           --base case, calculation end
    |otherwise = concat [num] (myCollatzIndex (collatz num))
    
-- | Q4
bisection :: (Double -> Double) -> (Double, Double) -> SF Double            --signum : return -1 for negative number and return 1 for positive number
bisection f (a,b)
    |((f a) < 0 && (f b ) < 0) || (f a) > 0 && (f b ) > 0 = FF      --if fa and fb are same sign then FF
    | a > b = bisection f (b,a)                                     -- if a > b then change the order, let min(a b) on right hand side
    | abs( f m)  < e = SS m                                         -- if fm < e then return m
    | signum (f a) /= signum (f m) = bisection f (a,m)              -- find the pair that has different sign and use it to do next calculation
    | signum (f b) /= signum (f m) = bisection f (b,m)
    |otherwise = FF 
    where 
        m = (a+b)/2

-- myBisec :: (Double -> Double) -> (Double, Double) -> SF Double
-- myBisec f (a,b)
--         | a > b = myBisec f (b,a)
--         | abs( f m)  < e = SS m
--         | ((f a )< 0) && (f m) > 0 = myBisec f (a,m)
--         | ((f a )> 0) && (f m) < 0 = myBisec f (m,a)
--         | ((f b )> 0) && (f m) < 0 = myBisec f (m,b)
--         | ((f b )< 0) && (f m) > 0 = myBisec f (b,m)   
--         |otherwise = FF 
--         where 
--             m = (a+b)/2


myappend :: [a] -> [a] -> [a]
myappend [] l = l
myappend (a:as) l = a : myappend as l

-- listCompare :: [a] -> [a] -> Bool 
-- listCompare [] [] = True 
-- listCompare  (x:xs) (y:ys)
--     | f x y = listCompare xs ys
--     | otherwise = False 

-- | Q5             in the test, make sure to know what kind of tree youve been given
bsort :: (a -> a -> Bool) -> [a] -> [a]                                 --for (int i = sizeof(list); i > 0; i--)
bsort f [] = []                                                                --bsort (x:xs):
--bsort f xs  =  myBsort f ( myBsort f xs)                                      -- if f ( list[i] list[i + 1]) = list[i] ++ bsort xs                    |compare and swap
bsort f xs = outLoop f xs (mylength xs)                                         -- if not f( list[i] list[i + 1]) = list[i+1] ++ bsort (list[i]:xs)     |
    where                                                                                           
        outLoop :: (a -> a -> Bool)  -> [a] -> Int  -> [a]      
        outLoop f xs 0 = xs
        outLoop f xs n = outLoop f (mybsort f xs) (n-1) 
            where
                mybsort :: (a -> a -> Bool)  -> [a]  -> [a]
                mybsort f [x] = [x]  
                mybsort f (a:b:xs)
                    | f a b  = a: mybsort f (b:xs)
                    | otherwise = b: mybsort f (a:xs)


-- myBsort :: (a -> a -> Bool)  -> [a]  -> [a]
-- myBsort f [x] = [x]  
-- myBsort f (a:b:xs)
--     | f a b  = a: myBsort f (b:xs)
--     | otherwise = myBsort f ( b: myBsort f (a:xs))


qsort :: (a -> a -> Bool) -> [a] -> [a]                     --choose the first element as pivot and compare with the rest of it
qsort f [] = []         
qsort f (x:xs) =
    let p1 = qsort f [y | y <- xs, f x y == True ]          --store the value that f x y == true            |and sort these two list until []
        p2 = qsort f [ y | y <- xs, f x y /= True ]         -- store the value that f x y /= true           |
    in concat p2 (x:p1 )                                    --concat two list





msort :: (a -> a -> Bool) -> [a] -> [a]
msort = undefined



-- | Q6
-- mapSf :: (a -> b) -> SF [a] -> SF [b]
-- mapSf f (SS x)  = SS (myMap f x )

--map function
myMap :: (a -> b) ->  [a] ->  [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

-- sfConcat :: SF [a] -> SF [a] -> SF [a]
-- sfConcat (SS xs) (SS ys) = SS (concat xs ys)


--check the dimension of a matrix return true if each line of the matrix are equal
checkDim :: Matrix a  -> Bool 
checkDim [x] = True 
checkDim (x:y:xs)
    | mylength x /= mylength y = False 
    | otherwise = checkDim (y:xs) 

transpose :: Matrix a -> SF (Matrix a)
transpose [] = SS []
transpose (x:xs)
    |checkDim (x:xs) == False = FF          --if the dimension of the matrix is not valid = FF
    |otherwise = SS (myTranspose (x:xs))        
    where
        myTranspose :: Matrix a -> Matrix a         --get the head of each line and concat them into a list, recursively call the function to calculate its tail's transpose
        myTranspose ([]:_) = []                       --until its empty
        myTranspose xs = (myMap myHead xs) : (myTranspose (myMap myTail xs))        


--get the first element of the list
myHead :: [a] -> a
myHead (x:xs) = x

--get the list 
myTail :: [a] -> [a]
myTail (x:xs) = xs


myAddRow :: (a -> b -> c) -> [a] -> [b] ->  [c]         --add two list together by adding each element
myAddRow f [] [] = []
myAddRow f (a:as) (b:bs) = f a b : myAddRow f as bs



addMat :: DoubleMatrix -> DoubleMatrix -> SF DoubleMatrix
addMat (x:xs) (y:ys)  
    |checkDim (x:xs) == False = FF          -- if x is not a valid matrx
    |checkDim (y:ys) == False = FF             -- if y is not a valid matrx
    |mylength x /= mylength y = FF          -- if x and y are not same size
    |otherwise = SS (myAddMat (x:xs) (y:ys))
    where
        myAddMat :: DoubleMatrix -> DoubleMatrix -> DoubleMatrix
        myAddMat [] [] = []
        myAddMat (xs:xss) (ys:yss) = myAddRow (+) xs ys : (myAddMat xss yss)     -- pass x and y's first line to myAddRow and add them together
                                                                                 -- recursively call the function keep adding each line

mylength :: [a] -> Int          --return the length of a list
mylength [x] = 1
mylength (x:xs) = 1 + mylength xs

--this is from Sept.29th tutorial by Haiyang



-- Zipping with two lists and applying a function f
myzipwith:: (a -> a -> a) -> [a] -> [a] -> [a]
myzipwith f [] [] = []
myzipwith f (x:xs) (y:ys) = (f x y) : (myzipwith f xs ys)

multMat :: DoubleMatrix -> DoubleMatrix -> SF DoubleMatrix
multMat (x:xs) (y:ys )
    |checkDim (x:xs) == False = FF      --if x is not a valid matrix
    |checkDim (y:ys) == False = FF      --if y is not a valid matrix
    |mylength x /= mylength (myMap myHead (y:ys)) = FF      --if size of x and y does not support multiplication
 --   |mylength y /= mylength (myMap myHead (x:xs)) = FF 
    |otherwise = SS (myMult (x:xs)  (mytranspose (y:ys)))
    where 
        mytranspose :: Matrix a -> Matrix a         --get the head of each line and concat them into a list recursively call the function to calculate its tail's transpose
        mytranspose ([]:_) = []                     
        mytranspose xs = (myMap myHead xs) : (mytranspose (myMap myTail xs))   --send the transposed matrix to do the multiplication

        myMult :: DoubleMatrix -> DoubleMatrix -> DoubleMatrix
        myMult [] _ = []
        myMult (x:xs) (y:ys) =  mycalculate x (y:ys) : (myMult xs (y:ys))       --calculate each row and concat together    
        
        -- where
        --     calLine = (myfoldr (+) 0 ( myzipwith (*) x y ) )  (myMult xs ys)
mycalculate ::(Num a) => [a] -> [[a]] -> [a]                        --calculate each line           myzipwith: multiply two list by multiply each corresponding element and return as a list
mycalculate _ [] = []                                                                               -- myfoldr；add each element in the list, which is a element in the answer matrix
mycalculate x (y:ys) = (myfoldr (+) 0 ( myzipwith (*) x y ) ): (mycalculate x ys)                   -- recursively do the calculation for the whole row
    where
        myfoldr f n [] = n
        myfoldr f n (a:as) = f a (myfoldr f n as)

--this is from http://pages.cpsc.ucalgary.ca/~robin/class/449/notes/lists2.hs
--it take the first element and concat with the answer of a recursive apply the function to the rest of list until the last element
--
-- | Q7.
nreverse :: [a] -> [a]
nreverse [x] = [x]
nreverse (x:xs) = concat (nreverse xs) [x]

--this is from http://pages.cpsc.ucalgary.ca/~robin/class/449/notes/lists2.hs
-- f takes two list, [a] and [b], get the first element with list a and add it to the front of [b], untill the [a] = []
--if [a] = [], return [b]
freverse :: [a] -> [a]
-- freverse = undefined
freverse l = f l []
    where
        f :: [a] -> [a] -> [a]
        f [] l = l
        f (a:as) l = f as (a:l)

-- x start from an empty list and recursively apply the function to the input list xs,
-- it brings element y to the front of list x, and concat them until input list is empty
-- 
hreverse :: [a] -> [a]      
hreverse xs = myfoldr [] (\x y -> concat y [x]) xs
    where
        myfoldr l f  [] = l
        myfoldr l f  (x:xs) = f x ( myfoldr l f  xs)
-- | Q8.
isAVL :: Ord a => STree a -> Bool           
isAVL Leaf = True                                       -- if it is leaf then = true
isAVL (Node Leaf v (Node Leaf v1 Leaf)) = (v < v1)                                                                                       --   v                             V
isAVL (Node (Node Leaf  v1 Leaf ) v Leaf) = (v1 < v)        -- comment →→→→                                                             --  /   \           OR            /    \
isAVL (Node left v right)                                                                                                               --Leaf  v1                       V1     Leaf      answer will depends on comparison between v and v1                        
    | (abs((depth left) - (depth right)) <= 1) && (increased (Node left v right)) && (isAVL left) && (isAVL right) = True               --      /\                     /   \                
    |otherwise = False         -- ↑ the height difference between of left subtree and right subtree needs to < 1                        --  Leaf  Leaf              leaf   Leaf
        where                   -- increased: check if node value fits the required ment, left subtree and right subtree needs to be a valid subtree
            increased :: Ord a => STree a -> Bool
            increased (Node Leaf v Leaf) = True
            increased (Node (Node left1  v1 right1 ) v (Node left2 v2 right2)) = ((v1 <= v) && (v <= v2)) 
           

--get the depth(height) of subtree, from lecture notes
depth :: STree a -> Integer
depth Leaf = 0
depth (Node left v right) = 1 + (max (depth left)(depth right ))


--it translate a 2d list into a single list
myConnect :: [[a]] -> [a]
myConnect [] = []
myConnect (x:xs) = concat x (myConnect xs)

-- this is from Oct.6th tutorial by Xi Wang
--it use map function recursively concat parent node with other nodes,
-- if its a [] it means its the leaf then return a 2d list, and use the function myConnect to expand the 2d list
--it use dfs way to go over each node of the tree
-- | Q9.
all_paths :: Rose a -> [[a]]            
all_paths (RS a rs) = go a rs
    where
        go a [] = [[a]]  -- returns a list of paths of a list of tose trees
        go a rs = myMap (a:) (myConnect (myMap all_paths rs)) -- [[a]]

-- | Q10.
factorialOf1891 :: Integer
factorialOf1891 = undefined

-- | Q11.
widthRose :: Rose a -> Int
widthRose (RS _ []) = 0
widthRose (RS a rs) = myHead ( bsort (>=) (longestPath (all_paths (RS a rs))))      --get each path, and convert into a list of int(each are it's length), and sort it to find the longest sublist
                                                                                    --then return the first element, which is the longest path
longestPath :: [[a]]  -> [Int]
longestPath [] = [] 
longestPath (x:xs) = (mylength x): longestPath xs

