{- 2) Algoritmo di euclide per l'MCD
        se m = 0 ho finito, l’mcd di m e n è n;
        se m < n scambia m e n;
        rimpiazza m con m − n e torna al passo 1.
-}

mcd :: Int -> Int -> Int
mcd 0 n = n
mcd m n = if m < n
	           then mcd n m
	           else mcd (m-n) n

{- 3) Elevamento a potenza
-}

(^^^) :: Int -> Int -> Int
x ^^^ 0 = 1
x ^^^ n | n > 0 && even n = x ^^^ (n `div` 2) * x ^^^ (n `div` 2)
        | otherwise       = x * x ^^^ (n `div` 2) * x ^^^ (n `div` 2)

{- 4) Fibonacci iterativo.
      Per calcolare il k-esimo numero di Fibonacci: 
      1- inizializza m a 0 e n a 1;
      2- se k = 0 ritorna il valore di m;
      3- se k = 1 ritorna il valore di n;
      4- rimpiazza m con n e n con m + n;
      5- decrementa k di 1 e torna al passo 2.
-}

fib :: Int -> Int
fib n = fibit n 0 1

fibit :: Int -> Int -> Int -> Int
fibit k m n | k == 0 = m 
            | k == 1 = n
            | otherwise = fibit (k-1) n (m+n)

{- 4) Permutazioni di una lista.
      Scrivere una funzione che calcola tutte le permutazioni degli
      elementi di una lista.
      > permutations [1,2,3]
      > [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

      Suggerimento: usare una funzione che dato un elemento ed una lista
      che lo contiene, restituisce la lista senza la prima occorrenza
      dell'elemento
-}

permutations :: Eq a => [a] -> [[a]]
permutations []  = [[]]
permutations [x] = [[x]]
permutations xs  = [x : ys | x <- xs, ys <- permutations (f x xs)]

f :: Eq a => a -> [a] -> [a]
f y (x:xs) = if x == y
	              then xs
	              else x : f y xs