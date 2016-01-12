{- 1) Consider a function safetail that behaves in the
      same way as tail, except that safetail maps the
      empty list to the empty list, whereas tail gives
      an error in this case. Define safetail using:
       (a) a conditional expression;
       (b) guarded equations;
       (c) pattern matching.
      Hint: the library function null :: [a] → Bool can
      be used to test if a list is empty.
-}

safetail1 :: [a] -> [a]
safetail1 xs = if null xs
	                then []
	                else tail xs
              
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs
            
safetail3 :: [a] -> [a]
safetail3 []     = []
safetail3 (x:xs) = tail (x:xs)

{- 2) Give three possible definitions for the logical
      or operator (||) using pattern matching.
-}

or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _     _     = True

or2 :: Bool -> Bool -> Bool
or2 True _  = True
or2 _ True  = True
or2 _    _  = False

(@@) :: Bool -> Bool -> Bool
False @@ False = False
_     @@ _     = True

{- 3) Redefine the following version of (&&) using
      conditionals rather than patterns:

      (##) :: Bool -> Bool -> Bool
      True ## True = True
      _    ## _    = False
-}

(##) :: Bool -> Bool -> Bool
b1 ## b2 = if b1 && b2
	            then True
	            else False

{- 3) Do the same for the following version:

      ($$) :: Bool -> Bool -> Bool
      True  $$ b = b
      False $$ _ = False
-}       

($$) :: Bool -> Bool -> Bool
b1 $$ b2 = if b1
	          then b2
	          else False

{- 5) Using library functions, define a function
      halve :: [a] -> ([a],[a])
      that splits an even-lengthed list into two halves.
      For example:
      > halve [1,2,3,4,5,6] ([1, 2, 3], [4, 5, 6])
-}

halve :: [a] -> ([a],[a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

{- 6) Show how the curried function definition
        mult x y z = x∗y∗z
      can be understood in terms of lambda expressions.
-}

mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z

{- 7) Valutare:
        Prelude > (\f x y -> f x y) (+) 3 4
                > 7

        Prelude > map (\x y -> (x * x) + (y * y)) [2,3,4]
                > [\y -> 4 + (y * y), \y -> 9 + (y * y), \y -> 16 + (y * y)]

                Potrei definire una funzione mapAll che prende due liste della stessa lunghezza
                (di cui la prima di funzioni) e applica ogni funzione della prima lista
                all'elemento corrispondente nella seconda.

                Ex: mapAll (map (\x y -> (x * x) + (y * y)) [2,3,4]) [2,3,4]
                    [8,18,32]

        Prelude > (\f g x y -> (x `f` x ) `g` (y `f` y )) (+) (*) 2 5
                > 40
-}

mapAll :: [(a->b)] -> [a] -> [b]
mapAll [] []         = []
mapAll (f:fs) (x:xs) = f x : (mapAll fs xs) 

{- 8) Cosa calcola la funzione j se

		g = \x −> x ∗ x
		h = \y −> g ( g y )
		j = h.h

	  j x calcola x^16. Es: 
	  > j 2
	  > 65536 
-}

