module Aula2 where
import Data.Char(ord,chr)
import Data.List
-- 1)

lInteiros :: [Int] -> Int -> [Int]
lInteiros [] a = []
lInteiros (x:y) a = ((x+a) : (lInteiros y a))

-- 2)

lStrings :: [String] -> Char -> [String]
lStrings [] a = []
lStrings (x:y) a = if ((take 1 (head (x:y))) == [a]) then (lStrings y a) else x:(lStrings y a) 


-- 3)

f3 :: [(Int,Int)] -> Int -> [(Int,Int)]
f3 [] a = []
f3 ((x1,y1):y2) a = ((x1+a, y1) : (f3 y2 a))

-- 4) Nesta função, temos que definir caso haja apenas um elemento e 
-- caso haja mais que um, verifica-se se este é superior ao resto da lista
-- invocando o resto da função no where, usando assim a recursividade

f4 :: [(Int,Int)] -> Int
f4 [] = error "não existe máximo numa lista vazia"
f4 [(x,y)]=y
f4 ((x1,y1):w) | y1 > m = y1
               | otherwise = m
                where m = f4 w

-- 5)

proxDigito :: Char -> Char
proxDigito a | ((ord a) > 47 && (ord a) < 58) = if (ord a) == 57 then '0' else chr (ord (a) + 1)
             | otherwise = error "Não é um algarismo"

-- 6)

subDigitos :: [Char] -> [Char]
subDigitos [] = []
subDigitos (x:y) = (chr (ord x + 1) : subDigitos (y))

-- 7)

proxVogal :: [Char] -> [Char]
proxVogal [] = []
proxVogal (x:y) | x == 'a' = ('e' : (proxVogal y))
                | x == 'e' = ('i' : (proxVogal y))
                | x == 'i' = ('o' : (proxVogal y))
                | x == 'o' = ('u' : (proxVogal y))
                | x == 'u' = ('a' : (proxVogal y))
                | otherwise = error "Não é uma vogal"

-- 8)

type Nome = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W deriving (Show,Eq)
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

-- a)

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao (Pos n (x,y)) [] = (Pos n (x,y))
posicao (Pos n (x,y)) l | head l == N = posicao (Pos n (x,y+1)) (tail l)
                        | head l == S = posicao (Pos n (x,y-1)) (tail l)
                        | head l == E = posicao (Pos n (x+1,y)) (tail l)
                        | head l == W = posicao (Pos n (x-1,y)) (tail l)
                        | otherwise = (Pos n (x,y))

-- b)

posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] _ = []
posicoesM (x:y) mov = ((posicao x [mov]) : (posicoesM y mov))

-- c) 

posicoesMs :: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs [] _ = []
posicoesMs _ [] = []
posicoesMs (x:y) mov = ((posicao x mov) : (posicoesMs y mov))

-- d)

ordenalista :: [PosicaoPessoa] -> Int
ordenalista l = head (reverse (sort (listays l)))
              where listays ((Pos n (x,y)):t) = (y : (listays t))
                    listays [] = []

pessoasNorte :: [PosicaoPessoa] -> [Nome]
pessoasNorte [] = []
pessoasNorte ((Pos n (x,y)):t) = if y == ordenalista ((Pos n (x,y)):t) then (n : pessoasNorte t) 
                                                                       else pessoasNorte t