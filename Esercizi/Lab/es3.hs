import Data.Char

prova :: IO (Char,Char)
prova = do x <- getChar
           getChar 
           y <- getChar
           return (x,y)

{- 1) Scrivere una funzione printSC che prende in input una
      stringa e un carattere e stampa (con print!) i caratteri della
      stringa eliminando il carattere dato come parametro.
-}

printSC :: [Char] -> Char -> IO ()
printSC xs x = print (rimuovi x xs)

rimuovi :: Char -> [Char] -> [Char]
rimuovi _ []     = []
rimuovi y (x:xs) = if (x == y)
	                    then xs
	                    else x : rimuovi y xs

{- 2) Scrivere una funzione rF che prende in input il nome di un
      file, legge il contenuto e lo mostra sul video.
-}

rF :: [Char] -> IO String
rF xs = readFile xs

{- 3) Scrivere una funzione ioFile che prende in input i nomi di
      2 files, legge una stringa sul primo file e la riscrive
      modificando i caratteri da minuscolo a maiuscolo sul
      secondo file. Suggerimento: usare toUpper.
-}

ioFile :: [Char] -> [Char] -> IO ()
ioFile xs ys = do zs <- readFile xs
                  writeFile ys (map toUpper zs)
                  return ()

{- 4) Scrivere una funzione rKwF che legge una stringa da
      tastiera e la memorizza in un file il cui nome è dato in input
      alla funzione.
-}

rKwF :: [Char] -> IO ()
rKwF xs = do putStrLn ("Scrivi qualcosa da memorizzare nel file " ++ xs ++ ":")
             ys <- getLine
             writeFile xs ys
             putStrLn ("Fatto!")
             return ()

{- 5) Scrivere una funzione rFl che restituisce il primo elemento
      di una lista di interi memorizzata come testo in un file.
      Suggerimento: read ss :: Type se ss è una stringa e Type un tipo
      restituisce il valore di tipo Type rappresentato dalla stringa ss(se possibile!).
      Ad esempio: ghci>	read "5" :: Int
                 5
                 ghci>	read "5" :: Float
                 5.0
                 ghci>	read "(3,'a')" :: (Int, Char)
                 (3,'a')
-}

rFl :: [Char] -> IO Int
rFl xs =  do ys <- readFile xs
             return (head (read ys :: [Int]))

{- 6) Scrivere una funzione factIO che, dopo aver scritto un
      messaggio di benvenuto, legge un numero, ne stampa il
      relativo fattoriale e resta in attesa di un altro numero, fino a
      che non viene immesso un carattere particolare che termina il
      dialogo. Il carattere di fine dialogo viene dato in input ad ogni
      chiamata della funzione. Suggerimento: per evitare a capo
      vedo "do" annidate si può usare la sintassi
      do {a1;...;an}
-}

factIO :: IO ()
factIO = do putStrLn "Benvenuto"
            x <- getChar
            putStrLn "\n"
            if (x /= 'q') 
               then do print (factorial (read [x] :: Int))
                       factIO
               else return ()

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)