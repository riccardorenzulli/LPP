import Data.Char

anagramma :: IO ()
anagramma = do putStrLn ("Insersci una parola")
               xs <- getLine
               yss <- readFile "listaAnagrammi.txt"
               putStrLn ("L'anagramma di " ++ xs ++ " è " ++ (checkAnagramma xs (read (yss))))

checkAnagramma :: [Char] -> [[Char]] -> [Char]
checkAnagramma xs xss = confrontaAnagrammi (permutations xs) xss

confrontaAnagrammi :: [[Char]] -> [[Char]] -> [Char]
confrontaAnagrammi [] _ = []
confrontaAnagrammi xss yss = if elem (head xss) yss then (head xss) else confrontaAnagrammi (tail xss) yss

permutations :: Eq a => [a] -> [[a]]
permutations []  = [[]]
permutations [x] = [[x]]
permutations xs  = [x : ys | x <- xs, ys <- permutations (f x xs)]

f :: Eq a => a -> [a] -> [a]
f y (x:xs) = if x == y
	              then xs
	              else x : f y xs