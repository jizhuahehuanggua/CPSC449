-- mycode f g [] = ([],[])
-- mycode f g (a:as) = case (mycode f g as) of 
--         (xs,ys) -> ((f a):xs, (g a):yx)

mycode f g = foldr(\x (as, bs) -> ((f x):as, (g x):bs)) ([],[])


--monade >>= 

sflist :: [SF a] -> (SF [a])
sflist [] = SS []
sflist (a:as) = do a' <- a 
                   as' <- sflist as 
                   return (a':as')

sflist (a:as) = a >>= (\a' -> (sflist as >>= (\as' -> (return (a':as')))))

sflist (a:as) = case a of 
    FF -> FF
    SS x -> f x
    where f x = case (sflist as) of
        FF -> FF
        SS xs -> SS (x:xs)

-- :r
wght Tp = 0
wght(Sn s1 a S2) = a + max (wght S1) (wght S2)

--wght tree = foldST g t tree
--从0开始, g is function that will applied to left and right child
wght = foldST (\ansl a ansr -> a + max ansl ansr) 0

--also alpha beta tree

α = -inf    best result from player
β = +inf    worst result from opponent
