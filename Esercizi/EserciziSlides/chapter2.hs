{- 1) Fix the syntax errors in the program below.
	  
  	  N = a 'div' length xs
            where
               a = 10
              xs = [1,2,3,4,5]
-}

n :: Int
n = a `div` length xs
    where a  = 10
          xs = [1,2,3,4,5]

{- 2) Show how the library function last that selects
      the last element of a list can be defined using
      the functions introduced in this lecture.
-}

last' :: [a] -> a
last' xs = head (reverse xs)

{- 3) Can you think of another possible definition?
      (These definition work also with empty list)
-}

safelast1 :: [a] -> [a]
safelast1 xs = drop (length xs - 1) xs

safelast2 :: [a] -> [a]
safelast2 [] = []
safelast2 [x] = [x]
safelast2 (x:xs) = safelast2 xs

{- 3) Similarly, show how the library function init
      that removes the last element from a list can
      be defined in two different ways.
-}

init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

safeinit1 :: [a] -> [a]
safeinit1 xs = take (length xs - 1) xs

safeinit2 :: [a] -> [a]
safeinit2 []     = []
safeinit2 [x]    = []
safeinit2 (x:xs) = x : safeinit2 xs

