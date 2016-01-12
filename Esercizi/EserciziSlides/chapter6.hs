{- (0) Define (with types!) and using recursion the
       functions that:
       a) calculate the sum of a list of numbers
       b) take a given number of elements from the start of a list
       c) select the last element of a non-empty list
       d) select the maximum element of a non-empty list
       e) repeat builds an infinite list of a given element
-}

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs

max1 :: Ord a => [a] -> a
max1 [x]    = x
max1 (x:xs) = if x > max1 xs then x else max1 xs

--più efficiente

max2 :: Ord a => [a] -> a
max2 [x]        = x
max2 (x1:x2:xs) = if x1 > x2 then max2 (x1:xs) else max2 (x2:xs)

repeat1 :: a -> [a]
repeat1 x = [x | _ <- [1..]]

repeat2 :: a -> [a]
repeat2 x = x : repeat2 x

{- 1) Without looking at the standard prelude, define
      the following library functions using recursion:
      a) Decide if all logical values in a list are true:
           and :: [Bool] -> Bool
      b) Concatenate a list of lists:
           concat :: [[a]] -> [a]
      c) Produce a list with n identical elements:
           replicate :: Int -> a -> [a]
      d) Select the nth element of a list:
           (!!) :: [a] -> Int -> a
      e) Decide if a value is an element of a list:
           elem :: Eq a => a -> [a] -> Bool
-}

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []  = []
concat' xss = head xss ++ concat' (tail xss)

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat1 x)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = if (x == y)
	                  then True
	                  else elem' y xs


{- 2) Define a recursive function
        merge :: [Int] -> [Int] -> [Int]
      that merges two sorted lists of integers to give
      a single sorted list. For example:
      > merge [2,5,6] [1,3,4]
      > [1,2,3,4,5,6]
-}

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                         then x : merge xs (y:ys)
                         else y : merge (x:xs) ys

{- 3) Define a recursive function
        msort :: [Int] -> [Int]
      that implements merge sort, which can be
      specified by the following two rules:
      Lists of length <= 1 are already sorted;
      Other lists can be sorted by sorting the two
      halves and merging the resulting lists.
-}

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge (msort (take (length (x:xs) `div` 2) (x:xs))) (msort(drop ((length (x:xs) `div` 2)) (x:xs)))