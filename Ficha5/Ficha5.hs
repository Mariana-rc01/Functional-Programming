module Ficha5 where

import Data.List

-- Exercício 1:

-- a) Confirmada na aula

anyy :: (a -> Bool) -> [a] -> Bool
anyy _ []    = False
anyy f (h:t) = f h || anyy f t

alll :: (a -> Bool) -> [a] -> Bool
alll _ []    = True
alll f (h:t) = f h && alll f t

-- b) Confirmada na aula

zipWithh :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithh _ [] _          = []
zipWithh _ _ []          = []
zipWithh f (h:t) (hh:tt) = f h hh : zipWithh f t tt

-- c) Confirmada na aula

takeWhilee :: (a -> Bool) -> [a] -> [a]
takeWhilee _ []    = []
takeWhilee f (h:t) = if f h then h:takeWhilee f t else []

-- d) Confirmada na aula

dropWhilee :: (a -> Bool) -> [a] -> [a]
dropWhilee _ []    = []
dropWhilee f (h:t) | f h = dropWhilee f t
                   | otherwise = (h:t)

-- e) Confirmada na aula

spann :: (a -> Bool) -> [a] -> ([a],[a])
spann _ []    = ([],[])
spann f (h:t) | f h        = (h:xs,ys)
              | otherwise  = ([],(h:t))
             where (xs,ys) = spann f t

-- f) Confirmada na aula

deleteByy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteByy _ _ []    = []
deleteByy f a (h:t) = if f a h then t else h:deleteByy f a t

-- g) Confirmada na aula

sortOnn :: Ord b => (a -> b) -> [a] -> [a]
sortOnn _ []    = []
sortOnn f (h:t) = insertOnn f h (sortOnn f t)

insertOnn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOnn _ x []    = [x]
insertOnn f x (h:t) | f x <= f h = x:h:t
                    | otherwise = h: insertOnn f x t

-- Exercício 2:

--Somatório dos elementos de uma lista

somatorio l = foldr (+) 0 l

type Polinomio = [Monomio]
type Monomio   = (Float,Int)

-- a) Confirmada na aula

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p

-- b) Confirmada na aula

conta :: Int -> Polinomio -> Int
conta n p = length $ filter (\x -> snd x == n) p

-- ou

conta' :: Int -> Polinomio -> Int
conta' n p = length $ selgrau n p

-- c) Confirmada na aula

grau :: Polinomio -> Int
grau p = snd $ last $ sortOn snd p

-- ou

grau' :: Polinomio -> Int
grau' p = maximum $ map snd p

-- ou

grau'' :: Polinomio -> Int
grau'' p = foldr(\(c,e) r -> max e r) 0 p

-- d)

deriv :: Polinomio -> Polinomio
deriv p = map (\(x,y) -> (x*fromIntegral y,y-1)) $ filter (\x -> snd x /= 0) p

-- e)

-- (mais complicada) foldl (\acc (n,g) -> acc + n*x^g) 0 p

calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(n,g) acc -> acc + n*x^g) 0 p

-- ou (intermedio)

calcula' :: Float -> Polinomio -> Float
calcula' x p = foldr (+) 0 (map f p)
             where f (c,e) = c*x^e

-- ou (mais simples)

calcula'' :: Float -> Polinomio -> Float
calcula'' x p = sum $ map (\(n,g) -> n*x^g) p

-- f)

simp :: Polinomio -> Polinomio
simp p = filter (\x -> fst x /= 0) p

-- g)

mult :: Monomio -> Polinomio -> Polinomio
mult (m1,m2) p = map (\(x,y) -> (m1*x,m2+y)) p

-- h)

ordena :: Polinomio -> Polinomio
ordena p = sortOn snd p

-- i)

normaliza :: Polinomio -> Polinomio
normaliza p = foldl (\acc m -> adiciona m acc) [] p
            where adiciona :: Monomio -> Polinomio -> Polinomio
                  adiciona m []              = [m]
                  adiciona (x,y) ((h1,h2):t) = if h2 == y then (h1+x,h2):t else (h1,h2):adiciona (x,y) t

-- j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = foldr (\m acc -> adiciona m acc) p1 p2
           where adiciona :: Monomio -> Polinomio -> Polinomio
                 adiciona m []              = [m]
                 adiciona (x,y) ((h1,h2):t) = if h2 == y then (h1+x,h2):t else (h1,h2):adiciona (x,y) t

-- k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldr (\m acc -> soma (mult m p2) acc) [] p1

-- l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)

-- Exercício 3:

type Mat a = [[a]]

m1 = [[1,2,3],[0,4,5],[0,0,6]]

-- a) Confirmada na aula

dimOK :: Mat a -> Bool
dimOK (h:t) = all (\l -> length l == length h) t

-- ou função nub retira elementos iguais, de modo que a função retira os tamanhos de todos
-- a nub fica apenas com um tamanho se a matriz estiver bem construída

dimOK' :: Mat a -> Bool
dimOK' = (== 1) . length . nub . map length

-- ou

dimOK'' :: Mat a -> Bool
dimOK'' m = let cl = length (head m)
            in and $ map (\l -> length l == cl) m 

-- b) Confirmada na aula

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat l@(h:t) = (length h, length l)

-- c) Assume-se que as matrizes têm a mesma dimensão
--    Confirmada na aula

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith $ zipWith (+)

-- d) Confirmada na aula

transpose' :: Mat a -> Mat a
transpose' m = [map (!! i) m | i <- [0..c-1]]
            where (l,c) = dimMat m

-- ou

transpose'' :: Mat a -> Mat a
transpose'' [] = []
transpose'' m  | null(head m) = []
               | otherwise    = (map head m) : transpose'' (map tail m) 

-- e) Confirmada na aula

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = map (\l -> [sum (zipWith (*) l [x !! i | x <- m2]) | i <- [0..c-1]]) m1
              where (l,_) = dimMat m1
                    (_,c) = dimMat m2

-- ou

multMat' :: Num a => Mat a -> Mat a -> Mat a
multMat' _ []     = []
multMat' [] _     = []
multMat' (a:b) m2 = multLinha a m : multMat' b m
                  where m = transpose m2

multLinha :: Num a => [a] -> Mat a -> [a]
multLinha l []     = []
multLinha l (a:as) = sum (zipWith (*) l a): multLinha l as

-- f)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' = zipWMat (+)

-- g)

triSup :: Real a => Mat a -> Bool
triSup m = and [all (== 0) [m !! i !! j | j <- [0..i-1]] | i <- [0..length m -1]]

-- h)

rotateLeft :: Mat a -> Mat a
rotateLeft m = [map (!! i) m | i <- [c-1,c-2..0]] 
            where (l,c) = dimMat m