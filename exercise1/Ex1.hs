-- | Exercise 1 template.
-- Instructions. 
--      1. Write your solutions to the exercise in this file. You
--      may add extra functions in this file to implement your solution.
--
--      2. Do NOT change the type or name of any of the template functions. 
--
--      3. Do NOT change the name of this file. 
--
--      4. Do NOT change the module name of this file.
--
--      5. To submit this to GradeScope, submit ONLY this file.
--
--      6. Have lots of fun :)
module Ex1 where

-- This includes the required types for this assignment (namely, the 'SF' type).
import Ex1Types

-- This imports some standard library functions for you to use. 
-- This imports some standard library functions for you to use. 
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), String, otherwise, abs, (+), (-), subtract, (*), (/), (==), (/=), (<), (<=), (>), (>=), (||), (&&), rem, mod, div, quot, max, min, fromIntegral, undefined, error, show, repeat)
-- This includes helpful functions for debugging.
import Debug.Trace

-- | Q1.  
avgThree :: Int -> Int -> Int -> Float
avgThree x y z = (fromIntegral x + fromIntegral y + fromIntegral z ) / 3.0
-- adding three integer convert to float and then divide by 3

-- | Q2. 
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree x y z
    | x == y && y == z = (x,3)      --if three int are equal
    | curMax > z = (curMax, 1)      --if the current max is greater than z, then it is the max
    | curMax == z = (z,2)           --if current max is equals to z but x != y then 
    | curMax < z = (z,1)            --if current max is smaller than z then z is the max
    where curMax = max x y  -- find the max between x and y


--ifac(int a, int b, int c)     a is factorial number, b is factorial number and c is the input integer
    --if a*b < c then we try new a = a*b new b = b + 1  ifac(int new a, int new b)
        --if ifac(int new a, int new b) > c then we return new b - 1, which is the old b        
ifac :: Integer -> Integer -> Integer -> SF Integer 
ifac a b c 
    | a*b < c = ifac (a*b) (b+1) c
    | a*b == c = SS b
    | a*b > c = SS (b-1)
-- | Q3. 
invFac :: Integer -> SF Integer
invFac x  
    | x < 0 = FF 
    | x == 0 || x == 1 = SS 1
    | otherwise = ifac 1 1 x
 -- if input integer is < 0 then no factorial number smaller than it
 --if input integer is 0 or 1, then the answer is 1
 --other wise we use ifac function to test out answer 



-- | Q4. 
myGcd :: Int -> Int -> Int
myGcd x y 
    | y == 0 = x                --if y == 0, x is the gcd, base case
    | x < 0 = myGcd (-x) y      --get absolute value, the mod functionm
    | y < 0 = myGcd x (-y)
    | x < y = myGcd y x         --needs to be the larger number mod smaller number
    | otherwise = myGcd y (x`mod`y) 
    

-- n!/(k! * (n-k)!)
fac :: Integer -> Integer 
fac 0 = 1
fac n = n * fac (n - 1) 
-- | Q5. 
binom :: Integer -> Integer -> Integer
binom n k
    | k == 0 = 1
    | otherwise = fac(n) `div`(fac(k) * fac(n-k))

concat :: [a] -> [a] -> [a]             
concat [] x = x
concat (x:xs) ys  = x : concat xs ys 


--gr(int[] repeat_times = (1,2,...n), String){ 
--  for(index i = 0; i < string.length; i++){repeat(repeat_times[i], string[i])} 
--    }
-- repreat(int n, char c){                  repeat character c n-times and return a string to gr
--   for(n,n > 0, n--){c = c:c}             gr pass n-time and charachter c to repeat function, 
-- }                                        concat all the string that returned from repeat together
gr :: [Integer] -> String -> String 
gr _ [] = []
gr (i:is) (x:xs) = concat (rrepeat i [x]) (gr is xs)

rrepeat :: Integer -> String -> String
rrepeat n [c]
    | n == 0 = []
    |otherwise = c : (rrepeat (n-1) [c])
-- | Q6. 
grow :: String -> String 
grow [] = []
grow st = gr [1,2..] st


-- test the list from the beginning, to the end. 
--isTrue = true
-- while(isTrue){ 
--      list are separate into three part: a = firat element, b = second element, and rest  
--      if (b > a){isTrue = false;}
--  }
-- | Q7. 
instrictorder :: [Int] -> Bool
instrictorder [a, b] = a < b
instrictorder(a:b:xs)
    | a <= b = instrictorder(b:xs)
    |otherwise = False 


--first = the first element in the tuple, second = the second element in the tuple
first :: (string, Int) -> string 
second :: (string, Int) -> Int 
second (a,b) = b
first (a,b) = a

-- use list comprehension return a list of element that element.second() < input integer
-- | Q8. 
cheapItems :: [(String, Int)] -> Int -> [String]
cheapItems [] budget = []
cheapItems (x:xs) budget
    | second x < budget = first x : cheapItems xs budget
    | second x >= budget = cheapItems xs budget

--same as Q8, but use the bubble sort, also the bubble sort algorithm is from the websites http://learnyouahaskell.com/recursion    
-- | Q9. 
sortByCost :: [(String, Int)] -> [(String, Int)]
sortByCost [] = []
sortByCost (x:xs) =
    let smallerSorted = sortByCost [a | a <- xs,second a <= second x]  
        biggerSorted = sortByCost [a | a <- xs, second a > second x]  
    in concat smallerSorted ( x : biggerSorted )


checkPrime :: [Integer] -> [Integer]
checkPrime [] = []
checkPrime (x:xs)  
    | [ z | z <- [2,3..x-1], x `mod` z == 0] /= [] =  checkPrime xs 
    | otherwise = x : checkPrime xs  
--use list comprehension to get a list of int that smaller than the input integer, and pass the list to checkPrime, 
--checkPrime: get a list of integr, only return the prime number from the list    
-- | Q10. 
divisors :: Integer -> [Integer]
divisors up_bound = checkPrime [ x | x <- [2,3..up_bound], up_bound `mod` x == 0]  


-- | Q11. 
substring :: String -> String -> Bool
substring [] y = True       
substring x [] = False          --if second string is finish but the first is not, means false
substring (x:xs) (y:ys) 
    | x == y = if subsubstring xs ys then True else substring (x:xs) ys         
    | otherwise  = substring (x:xs) ys         

subsubstring :: String -> String -> Bool
subsubstring [] y = True 
subsubstring x [] = False  
subsubstring (x:xs) (y:ys) =  x == y &&  subsubstring xs ys

--compare each char, if char x == string[b] y then keep comparing untill the first string is done
--if a != b repeat comparison a with string[b+1]


--keep adding sublist to each other
-- | Q12. 
sublists :: [a] -> [[a]]
sublists [] = [[]]  
sublists (x:xs) = concat (sublists xs) ([x : xx | xx <- sublists xs]) 