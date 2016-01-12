{- 1) Express the comprehension  [f x | x <- xs,  p x] using the
      functions map and filter.
-}

mapfilter :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapfilter f p xs = map f (filter p xs)

{- 2) Redefine map f and filter p using foldr.
-}

sum' = foldr (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

filter' ::  (a -> Bool) -> [a] -> [a]
filter' f = foldr ((++) . (\x -> if (f x) then [x] else [])) []

{- 3) Using foldl or foldl1, define a function dec2int :: [Int] -> Int that converts a  
      decimal number into an integer. For example:
      > dec2int [2,3,4,5]  
      2345
-}

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

{- 4) Explain  why  the  following  definition  is  invalid:
      sumsqreven =  compose  [sum,  map  (^2),  filter  even]

      Perchè le funzioni nella lista non hanno lo stesso tipo
-}

{- 5)  Define the higher-­order  library function curry that converts a function
       on pairs into a curried function, and, conversely, the function uncurry
       that converts a curried function with two arguments into a function
       on pairs
-}

add (x,y) = x + y
uadd x y = x + y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \(x,y) -> f x y

