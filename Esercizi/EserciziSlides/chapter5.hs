{- 1) Using a list comprehension, give an expression
      that calculates the sum 1^2 + 2^2 + . . . 100^2 of
      the first one hundred integer squares.
-}

sumsqr :: Int
sumsqr = sum[x^2 | x <- [1..100]]

{- 2) Show how the library function
        replicate :: Int -> a -> [ a ]
      that produces a list of identical elements can be defined using a list
      comprehension. For example:
      > replicate 3 True
      > [True, True, True ]
-}

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

{- 3) A triple (x,y,z) of positive integers is called
      pythagorean if x^2 + y^2 = z^2 . Using a list
      comprehension, define a function
        pyths :: Int -> [(Int,Int,Int)]
      that maps an integer n to all such triples with
      components in [1..n]. For example:
      > pyths 5
      [(3,4,5),(4,3,5)]
-}

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

{- 4) A positive integer is perfect if it equals the sum
      of all of its factors, excluding the number itself.
      Using a list comprehension, define a function
        perfects :: Int -> [Int]
      that returns the list of all perfect numbers up
      to a given limit. For example:
      > perfects 500
      [6,28,496]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], sum (init (factors x)) == x]

{- 5) Show how the single comprehension
        [(x,y) | x <- [1,2,3], y <- [4,5,6]]
      with two generators can be re-expressed using two comprehensions with
      single generators.
      Hint: make use of the library function concat and nest one comprehension
      within the other.
-}

multCompr :: [(Int,Int)]
multCompr = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

{- 6) Redefine the function positions using the function find.
-}

find :: Eq a => a -> [(a,b)] -> [b]
find k ts = [v | (k',v) <- ts, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions k ts = find k (zip ts [0..(length ts) - 1])

{- 7) The scalar product of two lists of integers xs
      and ys of length n is give by the sum of the
      products of the corresponding integers.
      Using a list comprehension, define a function
      that returns the scalar product of two lists.
-}

scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum[x*y | (x,y) <- zip xs ys]

{- 8) Cosa produce:

      > take 10 [ (i,j) | i <- [1,2], j <- [1..]]
      > [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
      e
      > take 5 [[ (i,j) | i <- [1,2]] | j <- [1..]]
      > [[(1,1),(2,1)],[(1,2),(2,2)],[(1,3),(2,3)],[(1,4),(2,4)],[(1,5),(2,5)]]
-}