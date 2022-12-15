module Ficha7 where

-- Exercício 1: Confirmado na aula

data ExpInt = Const     Int
            | Simetrico ExpInt
            | Mais      ExpInt ExpInt
            | Menos     ExpInt ExpInt
            | Mult      ExpInt ExpInt
            deriving Show

-- a) Confirmada na aula

calcula :: ExpInt -> Int
calcula (Const n)         = n
calcula (Simetrico e)     = -(calcula e)
calcula (Mais e1 e2)      = calcula e1 + calcula e2
calcula (Menos e1 e2)     = calcula e1 - calcula e2
calcula (Mult e1 e2)      = calcula e1 * calcula e2

-- b) Confirmada na aula

-- infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dê como resultado "(3 + (2 - 5))"

infixa :: ExpInt -> String
infixa (Const n)     = show n
infixa (Simetrico e) = "(-(" ++ infixa e  ++ "))"
infixa (Mais e1 e2)  = "("   ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "("   ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2)  = "("   ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

-- c) Confirmada na aula

-- "3 2 5 - +"

posfixa :: ExpInt -> String
posfixa (Const n)     = show n
posfixa (Simetrico e) = posfixa e  ++ " (-)"
posfixa (Mais e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

-- Exercício 2:

data RTree a = R a [RTree a]

-- R 2 [R 3 [R 4 []]]

-- a) Confirmada na aula

soma :: Num a => RTree a -> a
soma (R a as) = a + sum (map soma as)

-- b) Confirmada na aula

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a as) = 1 + maximum (map altura as)

-- c)

prune :: Int -> RTree a -> RTree a
prune 0 r        = r
prune n (R a as) = R a (map (prune (n-1)) as)

-- d)

mirror :: RTree a -> RTree a
mirror (R a as) = R a (map mirror (reverse as))

-- e)

postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a as) = (concatMap postorder as) ++ [a]

-- Exercício 3:

data LTree a = Tip a 
             | Fork (LTree a) (LTree a) 
             deriving Show

-- Fork (Tip 2) (Fork (Tip 3) (Tip 4))

-- a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a)    = a
ltSum (Fork e d) = ltSum e + ltSum d

-- b)

listaLT :: LTree a -> [a]
listaLT (Tip a)    = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

-- c)

ltHeight :: LTree a -> Int
ltHeight (Tip a)    = 0
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

-- Exercício 4:

data BTree a   = Empty  
               | Node a (BTree a) (BTree a)   
               deriving Show

data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b) 
               deriving Show

-- (No 2 (No 4 (Leaf "ola") (Leaf "Mariana")) (Leaf "Hugo"))

-- a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b)   = (Empty,Tip b)
splitFTree (No a e d) = (Node a eb db,Fork el dl)
                    where (eb,el) = splitFTree e
                          (db,dl) = splitFTree d

-- b)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip b)         = Just (Leaf b)
joinTrees (Node r e d) (Fork a b) = case (joinTrees e a,joinTrees d b) of (Just x,Just y) -> Just (No r x y)
                                                                          _               -> Nothing
joinTrees _ _                     = Nothing