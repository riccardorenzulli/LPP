import Data.Char
import Data.List
import Data.Typeable

{- 1) Scrivere una funzione ord1 che restituisce True se la lista cui viene applicata è ordinata
-}
ord1 :: Ord a => [a] -> Bool
ord1 []     = True
ord1 [x]    = True
ord1 (x:xs) = x < head xs && ord1 xs

{- 2a) Scrivere una funzione sottolisteF che realizza la seguente funzionalità:
       parametro: una lista l
       risultato: la lista di tutte le sottoliste finali di l
-}

sottolisteF :: [a] -> [[a]]
sottolisteF []     = [[]]
sottolisteF [x]    = [[x]]
sottolisteF (x:xs) = (x:xs) : sottolisteF xs

{- 2b) Scrivere una funzione sottolisteI che realizza la seguente funzionalità:
       parametro: una lista l
       risultato: la lista di tutte le sottoliste iniziali di l
-}

sottolisteI :: [a] -> [[a]]
sottolisteI []     = [[]]
sottolisteI [x]    = [[x]]
sottolisteI (x:xs) = (x:xs) : sottolisteI (take (length xs) (x:xs))

--oppure 
sottolisteI0 :: [a] -> [[a]]
sottolisteI0 []     = []
sottolisteI0 [x]    = [[x]]
sottolisteI0 (x:xs) = (x:xs) : sottolisteI0 (init (x:xs))

--oppure 
sottolisteI2 :: [a] -> [[a]]
sottolisteI2 xs = map reverse (sottolisteF (reverse xs))


{- 3) Scrivere una funzione sottostringa che realizza la
      seguente funzionalità:
      parametri: due stringhe s e l
      risultato: la posizione della prima occorrenza della
      sottostringa s in l
      (la prima posizione e' la posizione 0).
      Se s non compare in l la funzione restituisce -1.
-}

sottostringa :: [Char] -> [Char] -> Int 
sottostringa []     _      = 0
sottostringa xs ys = sottostringa' xs ys (length ys)

sottostringa'  _     [] n     = -n - 1
sottostringa' (x:xs) (y:ys) n = if x /= y
                                   then 1 + sottostringa' (x:xs) ys n
                                   else if prefix xs ys
                                           then 0 
                                           else 1 + sottostringa' (x:xs) ys n

prefix :: [Char] -> [Char] -> Bool
prefix []     _      = True
prefix _      []     = False
prefix (x:xs) (y:ys) = if x == y
                          then prefix xs ys
                          else False  

{- 4) Scrivere una funzione "parole" che realizza la seguente specifica:
      - parametro: una stringa s contenete caratteri e spazi
      - risultato: la lista di tutte le "parole" presenti in s, cioè le
      sequenze di caratteri contigui che occorrono in s senza spazi intermedi.
      esempio: parole "domani non si lavora" =
      ["domani","non","si","lavora"]
      Suggerimento: usare il predicato isSpace (import Data.Char)
-}

parole :: [Char] -> [[Char]]
parole [] = []
parole (x:xs) = if isSpace x 
	                 then parole xs 
	                 else [takeWhile (not . isSpace) (x:xs)] ++ parole (dropWhile (not . isSpace) (x:xs))

{- 5) Scrivere una funzione "uniocc" che realizza la seguente
      specifica:
      - parametro: un lista l
      - risultato: la lista di tutti gli elementi di l che occorrono
      UNA VOLTA SOLA in l
-}

uniocc :: Eq a => [a] -> [a]
uniocc [] = []
uniocc (x:xs) = if not (elem x xs) 
	                 then [x] ++ uniocc xs 
	                 else uniocc (filter (/=x) (x:xs))

{- 6) Scrivere una funzione infissa "prodM" che realizza il
      prodotto di due matrici di numeri rappresentate come liste di
      liste. La funzione deve verificare che le due matrici (cioè le
      loro rappresentazioni) siano ben formate e le loro dimensioni
      compatibili.
      NB: AB = C <-> #colonne A == #righe B e c avrà #righe A e #colonne B

      Ex: > prodM [[1,0,1],[1,5,-1],[3,2,0]] [[7,1],[1,0],[0,4]]
          > [[7,5],[12,-3],[23,3]]
          > prodM [[1,0,2],[0,3,-1]] [[4,1],[-2,2],[0,3]]
          > [[4,7],[-6,3]]
-} 

prodM :: Num a => [[a]] -> [[a]] -> [[a]]
prodM [[]] [[]] = [[]]
prodM xss yss = if (benFormata xss && benFormata yss && length (head xss) == length yss) 
	                 then let zss = init (trasposta yss)
                        in [prodVettoreRigaMatrice xs zss | xs <- xss]
	                 else error "Errore: #colonne della prima matrice deve essere uguale al #righe della seconda!"

-- restituisce un vettore
prodVettoreRigaMatrice :: Num a => [a] -> [[a]] -> [a]
prodVettoreRigaMatrice xs yss = [sum (zipWith (*) xs ys) | ys <- yss]

trasposta :: [[a]] -> [[a]]
trasposta ([]:_) = [[]]
trasposta xss    = map head xss : (trasposta (map tail xss))

benFormata :: [[a]] -> Bool
benFormata [[]] = True
benFormata xss = all (== length (head xss)) (map length xss)

{- 7) Scrivere una funzione "detM" per calcolare il determinante
      di una matrice quadrata.
      Suggerimento: usare lo sviluppo di Laplace

      N.B In questa versione fisso la prima riga
      Ex: > detM [[1,0,0],[0,2,0],[0,0,3]]
          > 6
-}

detM :: Num a => [[a]] -> a
detM [[]]  = error "La matrice non deve essere vuota"
detM xss   = det (length xss) (length (head xss)) xss

det :: Num a => Int -> Int -> [[a]] -> a
det 1 1 xss = head (head xss)
det n m xss = if (n == m) 
	               then sum ([((head xss) !! (k-1)) * ((-1)^(k+1)) * (det (n-1) (m-1) (nuovaMat (k-1) (tail xss))) | k <- [1..n]]) -- k è la colonna
                 else error "Errore: la matrice non è quadrata!"

nuovaMat :: Int -> [[a]] -> [[a]]
nuovaMat k  [] = []
nuovaMat k xss = [eliminaColonna k xs | xs <- xss] 

eliminaColonna :: Int -> [a] -> [a] 
eliminaColonna k xs = [fst p | ps <- [zip xs [0..]], p <- ps, (snd p) /= k]

{- 8) E’ possibile scrivere una funzione che:
      a) trasforma una lista in una tupla?
      	 No pero in alcuni casi si ma ovviamente avrà un tipo diverso 
      	 e devo sapere a priori la lunghezza della lista. Ex list2tuple [a] :: (a,a,a,a)

      b) trasforma una tupla di elementi dello stesso tipo in una lista?
      	 In genere no pero in alcuni casi si ma ovviamente avrà un tipo diverso 
      	 e devo sapere a priori la lunghezza della lista. Ex tuple2list :: (a,a,a,a) -> [a]

      c) controlla se 2 argomenti hanno lo stesso tipo?
         Sì, vedi checkArgs.
-}

checkArgs :: Typeable b => Typeable a => a -> b -> Bool
checkArgs a b = typeOf a == typeOf b

{- 9) Scrivere una funzione "modifica" che applicata ad una lista di coppie
      ed ad un carattere restituisca la lista di coppie il cui primo elemento è
      una stringa che contiene il carattere ed il secondo elemento è stato
      incrementato di 1.
-}

modifica :: Num a => [([Char],a)] -> Char -> [([Char],a)]
modifica []     _ = []
modifica (x:xs) n = [(ys,m+1) | (ys,m) <- (x:xs), elem n ys]

{- 10) Cosa calcola la funzione 

	\xs -> zip (map (\(y,z) -> y) xs) (map (\(y,z) -> z) xs)

	Prende come input una lista di coppie (y,z) e va a fare la zip su due liste: la prima lista
	contiene tutti i primi elementi delle coppie e la seconda tutti i secondi elementi delle coppie. 
	Quindi scompone la lista in input, la ricompone e restituisce la lista iniziale.
  Ex: > (\ xs -> zip (map (\(y,z) -> y) xs) (map (\(y,z) -> z) xs)) [(1,2),(3,4),(5,6)]
      > [(1,2),(3,4),(5,6)]
-}

{- 11) Scrivere una funzione "permCoppie" che applicata ad una lista di
       coppie con elementi di tipo diverso restituisca la lista di tutte le possibili
       coppie che includono le coppie della lista iniziale.
-}

permCoppie :: [(a,b)] -> [(a,b)]
permCoppie xs = [(x,y) | p1 <- xs, p2 <- xs, x <- [fst p1], y <- [snd p2]]

{- 12a) Scrivere una funzione "applyM" che applicata a 3 liste, di cui la
        prima contiene funzioni binarie, restituisca tutti i possibili risultati
        dell’applicazione delle funzioni nella prima lista agli elementi contenuti
        nelle altre 2 liste, supposte di tipo diverso.
        Ex: > applyM [(\x y -> (x,y))] [1,2,3,4] ['c','e']
            > [(1,'c'),(1,'e'),(2,'c'),(2,'e'),(3,'c'),(3,'e'),(4,'c'),(4,'e')]
-}

applyM :: [(a->b->c)] -> [a] -> [b] -> [c]
applyM fs xs ys = [f x y | f <- fs, x <- xs, y <- ys]

{- 12b) Scrivere la funzione "applyMS" che differisce da quella
        dell’esercizio precedente perché si suppone che la seconda e la terza
        lista abbiamo lo stesso tipo.
-}

applyMS :: [(a->a->a)] -> [a] -> [a] -> [a]
applyMS fs xs ys = [f x y | f <- fs, x <- xs, y <- ys]

{- 13) Scrivere una funzione "applyF" che applicata a 2 liste, di cui la prima
       contiene funzioni unarie, restituisca tutti i possibili risultati ottenuti
       componendo le funzioni della prima lista ed applicandole alla seconda
       lista.
       Ex: > applyF [(\x -> x+2),(\x -> x+2)] [1,2,3]
           > [5,6,7]
-}

applyF :: [(a->a)] -> [a] -> [a]
applyF fs xs = [ (foldl (.) (\ y -> y) fs) x | x <- xs]


{- 14) Scrivere una funzione "composeM" che applicata a 3 liste, di cui la
       prima contiene funzioni binarie, e le altre funzioni unarie, restituisca la
       lista di funzioni binarie ottenuta componendo una funzione binaria con 2
       funzioni unarie prese dalle 2 liste in tutti i modi possibili. Ad esempio se f
       appartiene alla prima lista e g alla seconda lista ed h alla terza lista, la
       lista risultato dovrà contenere la funzione \x y -> f (gx) (h y).

       Ex: applyM (composeM [max] [succ] [square]) [1,2,3] [4,5,6]
-}

composeM :: [(b->d->e)] -> [(b->b)] -> [(d->d)] -> [(b->d->e)]
composeM fs gs hs = [\x y -> f (g x) (h y) | f <- fs, g <- gs, h <- hs]

square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x * 2

{- 15) Scrivere una funzione "composeMS" che applicata a 2 liste, di cui la
       prima contiene funzioni binarie, e la seconda funzioni unarie, restituisca
       la lista di funzioni unarie ottenute componendo una funzione binaria con
       2 funzioni unarie in tutti i modi possibili. Ad esempio se f appartiene alla
       prima lista e g ed h alla seconda lista, la lista risultato dovrà contenere la
       funzione \x -> f (g x) (h x)
-}

composeMS :: [(a->a->b)] -> [(a->a)] -> [(a->b)]
composeMS fs gs = [\x -> f (g x) (h x) | f <- fs, g <- gs, h <- gs]

mapAll :: [(a->b)] -> [a] -> [b]
mapAll _      []     = []
mapAll []     _      = []
mapAll (f:fs) (x:xs) = f x : (mapAll fs xs) 

{- 16) Scrivere una funzione "parentesi" che applicata ad una stringa di
       parentesi controlli che sia ben formata restituendo True o False.

       Ex: > parentesi "(())(())((()))"
					 > True
					 > parentesi "(())((())()"
					 > False
-}

parentesi :: [Char] -> Bool

parentesi [] = True
parentesi (x:xs) = if (x=='(' && elem ')' xs)
	                    then parentesi (rimuovi ')' xs)
                      else False

rimuovi :: Char -> [Char] -> [Char]
rimuovi _ []     = []
rimuovi y (x:xs) = if (x == y)
	                    then xs
	                    else x : rimuovi y xs 