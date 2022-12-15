{-|
Module : Aula4
Description : Módulo Haskell contendo exemplos de funções recursivas
Copyright : Mariana Rocha <a90817@alunos.uminho.pt>

Este módulo contém definições Haskell para o cálculo de funções
recursivas simples.
-}
module Aula4 where
import Data.List

-- 1) Exercício da ficha1 - 1h):

{-| A função ´lNames´ recebe uma lista de nomes e retorna uma string
com a inicial do primeiro nome, seguida de um ponto, seguida do último nome.

A função poderia ser definida da seguinte forma:

@
lNames :: [String] -> String
lNames [] = ""
lNames l = take 1 (head l) ++ "." ++ last l
@ 

== Exemplos de utilização:

>>> lNames ["Joaquim", "Francisco", "Alves", "Martins"]
"J.Martins"
-}

lNames :: [String] -- ^ argumento: uma lista de nomes
             -> String -- ^ resultado: uma string
lNames [] = ""
lNames l = take 1 (head l) ++ "." ++ last l

-- 2) Exercício da ficha2 - 8c):

-- | Tipos de dados a representar a posição de uma pessoa num plano cartesiano
-- | Nome da pessoa
type Nome = String
-- | Coordenadas de uma pessoa
type Coordenada = (Int, Int)
-- | Norte, Sul, Este, Oeste
data Movimento = N | S | E | W  deriving (Show,Eq)
-- | Lista de movimentos
type Movimentos = [Movimento]
-- | Como se representa a posição de uma pessoa
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

{-| A função ´ordenalista´ é uma função auxiliar da ´pessoasNorte´.
Dada uma lista de posições de pessoas, função ´ordenalista´ retorna uma lista das ordenadas onde as pessoas se encontram de forma descrescente.

A função poderia ser definida da seguinte forma:

@
ordenalista :: [PosicaoPessoa] -> Int
ordenalista l = head (reverse (sort (listays l)))
              where listays ((Pos n (x,y)):t) = (y : (listays t))
                    listays [] = []
@
-}

ordenalista :: [PosicaoPessoa] -- ^ argumento: lista de posições de pessoas
                           -> Int -- ^ resultado: valores das abcissas das pessoas
ordenalista l = head (reverse (sort (listays l)))
              where listays ((Pos n (x,y)):t) = (y : (listays t))
                    listays [] = []

{-| Dada uma lista de posições de pessoas, função ´pessoasNorte´ retorna uma lista do nome das pessoas que se encontram mais a norte.
A função poderia ser definida da seguinte forma:

@
pessoasNorte :: [PosicaoPessoa] -> [Nome]
pessoasNorte [] = []
pessoasNorte ((Pos n (x,y)):t) = if y == ordenalista ((Pos n (x,y)):t) then (n : pessoasNorte t) else pessoasNorte t
@

== Exemplos de utilização:

>>> pessoasNorte [(Pos "Maria" (2,3)),(Pos "José" (1,4)),(Pos "Hugo" (3,5)),(Pos "Mariana" (4,5))]
["Hugo","Mariana"]
-}

pessoasNorte :: [PosicaoPessoa] -- ^ argumento: lista de posições de pessoas
                             -> [Nome] -- ^ resultado: lista de nomes
pessoasNorte [] = []
pessoasNorte ((Pos n (x,y)):t) = if y == ordenalista ((Pos n (x,y)):t) then (n : pessoasNorte t) else pessoasNorte t

-- 3) Exercício da ficha3 - 7):

{-| A função ´subs´ irá receber uma lista, uma certa posição e um elemento. Esta substitui o elemento na posição dada na lista.
Caso o valor da posição dada for superior ao tamanho da lista, a função irá retornar a lista. 

A função poderia ser definida da seguinte forma:

@
subs :: Eq a => [a] -> Int -> a -> [a]
subs [] _ _ = []
subs l pos elemento | length l < pos = l
                    | pos == 0 = [elemento] ++ (tail l)
                    | otherwise = (take (pos-1) l) ++ [elemento] ++ (drop pos l)
@ 

== Exemplos de utilização:

>>> subs "ola" 0 'O'
"Ola"

>>> subs [1,2,3,4] 2 5
[1,5,3,4]

>>> subs [1,2,3,4] 5 6
[1,2,3,4]
-}

subs :: Eq a => [a] -> Int -> a -- ^ argumentos: uma lista, a posição da substituição e valor dado
                            -> [a] -- ^ resultado: uma lista
subs [] _ _ = []
subs l pos elemento | length l < pos = l
                    | pos == 0 = [elemento] ++ (tail l)
                    | otherwise = (take (pos-1) l) ++ [elemento] ++ (drop pos l)