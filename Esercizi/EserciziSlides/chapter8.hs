data Shape = Circle Float | Rect Float Float | TriangleEq Float

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rect x y) = (x+y)*2
perimeter (TriangleEq l) = l*3

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rect x y) = x*y
area (TriangleEq l) = (l*h)/2 
    where h = sqrt (l^2 - (l/2)^2)

type Name = String
type Age = Int

data People = Person Name Age
              deriving (Eq,Show)

showPerson :: People -> String
showPerson (Person n a) = n ++ "--" ++ show a

{- Write examples showing different trees of the above types and for
   each type define a function occurs and a function flatten
		
	 (Node (Node (Leaf 5) 6 (Leaf 7)) 8 (Leaf 10))
   (Node1 (Node1 (Leaf1 1) (Leaf1 4)) (Node1 (Leaf1 6) (Leaf1 9)))
   (Node2 (Node2 (Leaf2) 1 (Leaf2)) 3 (Node2 (Leaf2) 2 (Leaf2)))
   (Node3 (Node3 (Leaf3 'a') 1 (Leaf3 'b')) 3 (Node3 (Leaf3 'c') 2 (Leaf3 'd')))
   (Node4 5 [(Node4 3 []), (Node4 1 [Node4 2 [], Node4 7 []])])

-}

data Tree = Leaf Int | Node Tree Int Tree
            deriving (Show)
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)  
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)  
data Tree4 a = Node4 a [Tree4 a]

occurs1 :: Eq a => a -> Tree1 a -> Bool
occurs1 m (Leaf1 n)   = m == n
occurs1 m (Node1 l r) = occurs1 m l || occurs1 m r

flatten1 :: Tree1 a -> [a]
flatten1 (Leaf1 a)   = [a]
flatten1 (Node1 l r) = flatten1 l ++ flatten1 r

occurs2 :: Eq a => a -> Tree2 a -> Bool
occurs2 m (Leaf2)       = False
occurs2 m (Node2 l n r) = m == n || occurs2 m l || occurs2 m r

flatten2 :: Tree2 a -> [a]
flatten2 (Leaf2)       = []
flatten2 (Node2 l n r) = [n] ++ flatten2 l ++ flatten2 r

occurs3l :: Eq a => a -> Tree3 a b -> Bool
occurs3l m (Leaf3 n)     = m == n
occurs3l m (Node3 l n r) = occurs3l m l || occurs3l m r

occurs3n :: Eq b => b -> Tree3 a b -> Bool
occurs3n m (Leaf3 n)     = False
occurs3n m (Node3 l n r) = m == n || occurs3n m l || occurs3n m r

occurs4 :: Eq a => a -> Tree4 a -> Bool
occurs4 m (Node4 n xs) = m == n || (or [occurs4 m x | x <- xs])

flatten4 :: Tree4 a -> [a]
flatten4 (Node4 n []) = n : []
flatten4 (Node4 n (x:xs)) = n : flatten4' (x:xs)

flatten4' [] = []
flatten4' (x:xs) = flatten4 x ++ flatten4' xs

{- 1) Using recursion and the function add, define a  
      function that multiplies two natural numbers.
-}

data Nat = Zero | Succ Nat
           deriving (Show)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult n Zero = Zero
mult (Succ m) (Succ n) = add (Succ m) (mult (Succ m) n)

mult' :: Int -> Int -> Int
mult' 0 _ = 0
mult' _ 0 = 0
mult' m n = m + mult' m (n-1)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
            deriving (Show)

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


{- 3) A binary tree is complete if the two sub-­trees of  
      every node are of equal size.
      Define a function that decides if a binary tree is complete.

      Ex: > isComplete (Node (Node (Node (Leaf 3) 1 (Leaf 2)) 3 (Leaf 4)) 2 (Leaf 1))
          > False

          > isComplete (Node (Node (Leaf 1) 3 (Leaf 2)) 2 (Node (Leaf 1) 3 (Leaf 2)))
          > True
-}

isComplete :: Tree -> Bool
isComplete (Leaf n)     = True
isComplete (Node l n r) = size l == size r && isComplete l && isComplete r

size :: Tree -> Int
size (Leaf n)     = 1
size (Node l n r) = 1 + size l + size r 

{- 4) The standard library defines 
                 data Ordering  =  LT | EQ | GT  
      together with a function
                 compare  ::  Ord  a  => a -> a -> Ordering  
      that decides if one value in an ordered type is less than (LT),  
      equal to (EQ), or greater than (GT) another such value.
      Using this function, redefine the function
                 occurs  ::  Int  -> Tree -> Bool  
      for search trees.
      Why is this new definition more efficient than the original version?  
-}

occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) | m == n = True
                      | m < n  = occurs m l
                      | m > n  = occurs m r

occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n)     = compare m n == EQ
occurs' m (Node l n r) | compare m n == EQ = True
                       | compare m n == LT = occurs' m l
                       | compare m n == GT = occurs' m r

{- 5) Consider the following type of binary trees:  
            data Tree = Leaf Int | Node Tree Tree  
      Let us say that such a tree is balanced if the number of leaves  
      in the left and right subtree of every node differs by at most one,  
      with leaves themselves being trivially balanced. Define a function  
            balanced :: Tree -> Bool 
      that decides if a tree is balanced or not.  
      Hint: first define a function that returns the number of leaves in  
      a tree.
-} 

data Tree5 = Leaf5 Int | Node5 Tree5 Tree5

balanced :: Tree5 -> Bool
balanced (Leaf5 n)   = True
balanced (Node5 l r) = if (abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r) 
	                        then True
	                        else False

numLeaves :: Tree5 -> Int
numLeaves (Leaf5 n)   = 1
numLeaves (Node5 l r) = numLeaves l + numLeaves r

{-(6) Define a function  
            balance :: [Int] -> Tree  
      that converts a non-­empty list of integers into a balanced tree.  
      Hint: first define a function that splits a list into two halves  
      whose length differs by at most one.  
-}

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs  = (Node (balance (fst (split (tail xs)))) (head xs) (balance (snd (split (tail xs)))))

split :: [Int] -> ([Int],[Int])
split xs = splitAt ((length xs) `div` 2) xs





