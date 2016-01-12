{- 1) Show that sum [x] = x for any number x.
-}

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

{- 2) Define a function product that produces the product of a list of numbers, and
      show using your definition that product [2,3,4] = 24.
-}

product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs


{- 3) How should the definition of the function qsort be modified so that it produces a
      reverse sorted version of a list?
-}

qsort1 :: Ord a => [a] -> [a]
qsort1 []     = []
qsort1 (x:xs) = qsort1 smaller ++ [x] ++ qsort1 larger
    where
    	smaller = [a | a <- xs, a > x]
    	larger  = [b | b <- xs, b <= x]


{- 4) Q: What would be the effect of replacing <= by < in the definition of qsort?
      R: An ordered list without duplicates
-}

qsort2 :: Ord a => [a] -> [a]
qsort2 []     = []
qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
    where
    	smaller = [a | a <- xs, a < x]
    	larger  = [b | b <- xs, b > x]