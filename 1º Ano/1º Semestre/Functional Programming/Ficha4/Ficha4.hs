module Ficha4 where
import Data.Char
import Data.List

-- Exercício 1: Confirmada na aula

digitAlpha :: String -> (String,String)
digitAlpha s = digitAlphaAc ([],[]) s
            where digitAlphaAc :: (String,String) -> String -> (String,String)
                  digitAlphaAc ac [] = ac
                  digitAlphaAc (l,ls) (h:t) = if isDigit h then digitAlphaAc (l,ls++[h]) t 
                                                           else digitAlphaAc (l++[h],ls) t


-- Exercício 2: Confirmada na aula

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0  = (1+x,y,z)
          | h == 0 = (x,1+y,z)
          | h > 0  = (x,y,1+z)
          where (x,y,z) = nzp t

-- com acumuladores

nzp' :: [Int] -> (Int,Int,Int)
nzp' l = nzpAc (0,0,0) l
       where nzpAc :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
             nzpAc (x,y,z) [] = (x,y,z)
             nzpAc (x,y,z) (h:t) | h < 0  = nzpAc (x+1,y,z) t
                                 | h == 0 = nzpAc (x,y+1,z) t
                                 | h > 0  = nzpAc (x,y,z+1) t


-- Exercício 3: Confirmada na aula

{-15 / 5 = (3,0) => 15-5=10 (1,0) 10>5,10-5=5 (2,0) 5=5,5-5=0 (3,0) 0<5
 16/5 = (3,1) => 16-5=11 (1,0) e 11>5, 11-5=6 (2,0) 6>5, 6-5=1 (3,1) 1<5
 4/2 = (2,0) => 4-2=2 (1,0) e 2=2
 2/4 = (0,2)-}

divMod' :: Integral a => a -> a -> (a,a)
divMod' a b = divModd a b (0,0)

divModd :: Integral a => a -> a -> (a,a) -> (a,a)
divModd 0 _ (x,y) = (0,0)
divModd a b (x,y) | a < b      = (x,a)
                  | (a-b) > b  = divModd (a-b) b (x+1,y)
                  | (a-b) == b = divModd (a-b) b (x+1,0)
                  | (a-b) < b  = (x+1,a-b)

-- ou

divModa :: Integral a => a -> a -> (a,a)
divModa m n | m == n = (1,0)
            | n > m  = (0,m)
            | otherwise = let (d,r) = divModa (m-n) n 
                          in  (d+1,r)

-- Exercício 4: Confirmada na aula
{-
Pretende-se passar de [1,2,3,4] para 1234 como algarismo
Nota: 1 × 10^3 + 2 × 10^2 + 3 × 10^1 + 4 × 10^0 = 4 + 10 × (3 + 10 × (2 + 10 × (1 + 10 × 0)))
= (((1 + 10*0)*10 + 2)*10 + 3)*10 + 4)
-}

fromDigits :: [Integer] -> Integer
fromDigits l = aux l 0
    where aux [] a      = a
          aux (x:xs) a  = aux xs ((a * 10) + x)


-- Exercício 5:
{-
Pretende-se fazer uma função que receba um init de l, veja se é maior que o seguinte, se for guarda o init, senão guarda
o outro e faz a função com o init guardado
-}

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSum (inits l)


maxSum :: (Num a, Ord a) => [[a]] -> a
maxSum []  = 0
maxSum [x] = sum x
maxSum (h:h1:t) | sum h1 > sum h = maxSum (h1:t)
                | otherwise      = maxSum (h:t)

-- Exercício 6:
{- Dada uma função fib' que recebe dois acumuladores a e b, esta calcula o fib' em a e b (fib' (a,b) n = fib' (b,a+b), ie,
fib(n) = f(n-1) + fib(n-2)) e portanto depois damos os valores a=0 e b=1, pois é o valor de fib 0 e o valor de fib 1.
-}

fib :: Int -> Int
fib n = fib' (0,1) n
    where fib' (a,b) 0 = a
          fib' (a,b) 1 = b
          fib' (a,b) n = fib' (b,a+b) (n-1)

{-
Exemplo: fib 5 = fib' (0,1) 5 = fib' (1,1) 4 = fib' (1,2) 3 = fib' (2,3) 2 = fib' (3,5) 1 = 5
-}

-- Exercício 7: quotRem 1234 10 = (123,4) Rever esta, porque uso duas funções auxiliares!!!
-- A função auxiliar recebe um número inteiro e devolve uma lista com cada algarismo como um elemento da lista
-- A função str pega na lista e adiciona os elementos da lista numa string

intToStr :: Int -> String
intToStr x = str (auxiliar x)
          where str (h:t) = chr(h+48):(str t)
                str []    = []

auxiliar :: Int -> [Int]
auxiliar x | x <= 9    = [r]
           | otherwise = auxiliar q ++ [r]
          where (q,r)  = quotRem x 10

-- Somatório com acumuladores:

somatorioAc :: [Int] -> Int
somatorioAc l = somatorioAc' 0 l
              where somatorioAc' :: Int -> [Int] -> Int
                    somatorioAc' ac []    = ac
                    somatorioAc' ac (h:t) = somatorioAc' (ac+h) t