import Parserccs
import Data.Tree
import Data.Tree.Pretty

{- 
  
   Il programma consiste nei seguenti passi:
   1) viene letto un programma ccs in un file txt (> ccs "file.txt")
   2) tale programma viene parsificato e come output vengono restituiti una symbol table e un processo che contiene i processi da mettere in parallelo
   3) l'inteprete ha come input la symbol table e il processo e viene restituito l'albero con i passi di riduzione

-}

ex1CMCS = interpreter [("CS", OutputAction "coin" (InputAction "coffee" (Id "CS"))), ("CM", InputAction "coin" (OutputAction "coffee" (Id "CM")))] (Par [Id "CS", Id "CM"]) (-1,-1) 10
ex2CSCTM = interpreter [("CTM", InputAction "coin" (ExtChoice (OutputAction "coffee" (Id "CTM")) (OutputAction "tea" (Id "CTM")))), ("CS", OutputAction "coin" (InputAction "coffee" (Id "CS")))] (Par [Id "CS", Id "CTM"]) (-1,-1) 5
ex3CSCTMCS' = interpreter [("CTM", InputAction "coin" (ExtChoice (OutputAction "coffee" (Id "CTM")) (OutputAction "tea" (Id "CTM")))), ("CS", OutputAction "coin" (InputAction "coffee" (Id "CS")))] (Par [Id "CS", Id "CTM", OutputAction "coin" (InputAction "tea" Nil)]) (-1,-1) 5
ex4ATMC = interpreter [("ATM", InputAction "carta" (InputAction "pin" (Id "ATM'"))), ("ATM'",ExtChoice (ExtChoice (ExtChoice (InputAction "prelievo" (Id "ATM'")) (InputAction "versamento" (Id "ATM'"))) (InputAction "conto" (Id "ATM'")))(InputAction "esci" (Id "ATM"))), ("C",OutputAction "carta" (OutputAction "pin" (OutputAction "conto" (OutputAction "prelievo" (OutputAction "esci" Nil)))))] (Par [Id "ATM", Id "C"]) (-1,-1) 5

prova1 = interpreter [("CS",(InputAction "coffee" (InputAction "tea" (Id "CS")))), ("CM", (OutputAction "coffee" (OutputAction "tea" (Id "CM"))))] (Par [(InputAction "coffee" (InputAction "tea" (Id "CS"))), (OutputAction "coffee" (OutputAction "tea" (Nil))), OutputAction "coffee" (InputAction "chocolate" (Nil))]) (-1,-1) 5
prova2 = interpreter [("CS",(InputAction "coffee" (InputAction "tea" (Id "CS")))), ("CM", (OutputAction "coffee" (OutputAction "tea" (Nil))))] (Par [Id "CS",Id "CM"]) (-1,-1) 5
prova3 = interpreter [("CS",(InputAction "coffee" (InputAction "tea" (Id "CS"))))] (Par [ExtChoice (InputAction "a" Nil) (InputAction "b" Nil), ExtChoice (OutputAction "a" Nil) (OutputAction "b" Nil), Id "CS", OutputAction "coffee" Nil]) (-1,-1) 1

-- Legge un programma ccs, lo parsifica e lo interpreta
ccs :: [Char] -> IO ()
ccs xs = do ys <- readFile xs
            let (symT,process) = parser (lexer (filter ( /= '\n') ys)) in putStrLn $ drawTree $ trasform (interpreter symT process (-1,-1) 8)

{- Input:  -symbol table
           -processo Par [P1,P2,..,Pn] dove P1,P2,..,Pn sono i processi da mettere in parallelo
           -una coppia di interi che indicano i due processi che stiamo riducendo (utili per la colorazione)
           -un intero che indica il massimo numero di passi di riduzione
   Output: albero con i passi di riduzione
-}
interpreter :: [([Char],Process)] -> Process -> (Int,Int) -> Int -> Tree (Process, (Int,Int))
interpreter symT (Par xs) (a,b) n = let processReducedOneStep = Node (Par xs, (a,b)) [interpreter symT z (x,y) (n-1) | x <- [0..(length xs)-1], y <- [x+1..(length xs)-1], (z,u) <- reduce symT (x,y) xs, u, n /= 0] 
                                    in (processReducedOneStep)

-- Prova a ridurre due processi
reduce :: [([Char],Process)] -> (Int,Int) -> [Process] -> [(Process,Bool)]
reduce symT (x,y) xs = let zs = evaluate2Proc symT (xs !! x, xs !! y)
                       in if null zs
                             then [(Nil,False)]
                             else [(Par (substitute y (snd z) (substitute x (fst z) xs)),True) | z <- zs] 

evaluate2Proc :: [([Char],Process)] -> (Process,Process) -> [(Process,Process)]
evaluate2Proc symT (InputAction az1 p1,p2)  = case p2 of OutputAction az2 p3 -> if (az1 == az2) then [(p1,p3)] else []
                                                         ExtChoice p3 p4     -> evaluate2Proc symT (InputAction az1 p1,p3) ++ evaluate2Proc symT (InputAction az1 p1,p4)
                                                         Id id1              -> evaluate2Proc symT (InputAction az1 p1,(search symT id1))
                                                         _                   -> []

evaluate2Proc symT (OutputAction az1 p1,p2) = case p2 of InputAction az2 p3  -> if (az1 == az2) then [(p1,p3)] else []
                                                         ExtChoice p3 p4     -> evaluate2Proc symT (OutputAction az1 p1,p3) ++ evaluate2Proc symT (OutputAction az1 p1,p4)
                                                         Id id1              -> evaluate2Proc symT (OutputAction az1 p1,(search symT id1))
                                                         _                   -> []

evaluate2Proc symT (ExtChoice p3 p4,p2)     = case p2 of InputAction az2 p5  -> evaluate2Proc symT (p3,p2) ++ evaluate2Proc symT (p4,p2)
                                                         OutputAction az2 p5 -> evaluate2Proc symT (p3,p2) ++ evaluate2Proc symT (p4,p2)
                                                         Id id1              -> evaluate2Proc symT (ExtChoice p3 p4,(search symT id1))
                                                         ExtChoice p5 p6     -> evaluate2Proc symT (p3,p5) ++ evaluate2Proc symT (p3,p6) ++ evaluate2Proc symT (p4,p5) ++ evaluate2Proc symT (p4,p6) 
                                                         _                   -> []

evaluate2Proc symT (Id id1,p2)              = case p2 of Id id2 -> evaluate2Proc symT ((search symT id1),(search symT id2))
                                                         Nil    -> []
                                                         _      -> evaluate2Proc symT ((search symT id1),p2)

evaluate2Proc symT (Nil,p2)                 = []

evaluate2Proc symT (p1,p2)                  = error "Processo sconosciuto"

-- Controlla che il processo passato in input sia presente nella symbol table
checkSymbleTable :: [([Char],Process)] -> [Char] -> Bool
checkSymbleTable xs ys = if elem ys zs
	                          then True
	                          else False
	                       where zs = [x | (x,y) <- xs]

-- Se il processo passato in input è presente nella symbol table allora viene restituita la sua definizione,
-- altrimenti viene lanciato un errore
search :: [([Char],Process)] -> [Char] -> Process
search [] _ = error "Symble table vuota"
search symT xs = if checkSymbleTable symT xs then head zs else error "Processo non definito"
   where zs = [y | (x,y) <- symT, x == xs]

-- Sostituisce il processo in posizione x con il processo y nella lista di processi
substitute :: Int -> a -> [a] -> [a]
substitute x y (z:zs) | x == 0    = y : zs
                      | otherwise = z : substitute (x-1) y zs

-- Trasforma un albero di processi (con le relative coppie di processi ridotti) in un albero di stringhe
trasform :: Tree (Process, (Int,Int)) -> Tree [Char]
trasform (Node (Par ys, (a,b)) xs) = Node (trasformRoot ys (a,b)) [trasform x | x<-xs]

trasformRoot :: [Process] -> (Int,Int) -> [Char]
trasformRoot [] (a,b) = "\x1b[39m" ++ []
trasformRoot (x:xs) (a,b) = if a == 0 || b == 0 then  "\x1b[1;31m" ++ trasformNode x ++ "\x1b[0;37m" ++ (if null xs then trasformRoot xs (a-1,b-1) else " | " ++ trasformRoot xs (a-1,b-1))
                                                else "\x1b[0;37m" ++ trasformNode x ++ "\x1b[0;37m" ++ (if null xs then trasformRoot xs (a-1,b-1) else " | " ++ trasformRoot xs (a-1,b-1))

trasformNode :: Process -> [Char]
trasformNode (InputAction a p1) = a ++ "." ++ trasformNode p1
trasformNode (OutputAction a p1) = "!" ++ a ++ "." ++ trasformNode p1
trasformNode (ExtChoice p1 p2) = "(" ++ trasformNode p1 ++ " + " ++ trasformNode p2 ++ ")"
trasformNode (Id xs) = xs
trasformNode (Nil) = "0"