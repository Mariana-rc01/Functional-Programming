{-|
Module : Aula3
Description : Módulo Haskell contendo exemplos de funções recursivas
Copyright : Mariana Rocha <a90817@alunos.uminho.pt>

Este módulo contém definições Haskell para o cálculo de funções
recursivas simples.
-}
module Aula3 where

--Exercício 2:

{-| Dada uma lista l e um número inteiro a, a função ´descolad´ desloca cada elemento da lista a posições para a direita.

A função poderia ser definida da seguinte forma:

@
deslocad :: [a] -> Int -> a
deslocad [] _ = []
deslocad l a | a == 1 = [last l] ++ (init l)
             | otherwise = (deslocad ([last l] ++ (init l)) (a-1))
@ 

== Exemplos de utilização:

>>> deslocad [1,2,3,4,5] 2
[4,5,1,2,3]

-}

deslocad :: [a] -> Int -- ^ argumentos: uma lista e um inteiro (não negativo)
              -> [a] -- ^ resultado: uma lista
deslocad [] _ = []
deslocad l a | a == 1 = [last l] ++ (init l)
             | otherwise = (deslocad ([last l] ++ (init l)) (a-1))

-- Exercício 3:

{-| Dada uma lista l e um número inteiro a, a função ´descolae´ desloca cada elemento da lista a posições para a esquerda.

A função poderia ser definida da seguinte forma:

@
deslocae :: [a] -> Int -> [a]
deslocae [] _ = []
deslocae (h:t) a | a == 0 = t ++ [h]
                 | otherwise = (deslocae (t ++ [h]) (a-1))
@ 

== Exemplos de utilização:

>>> deslocae [1,2,3,4,5] 2
[4,5,1,2,3]

-}

deslocae :: [a] -> Int -- ^ argumentos: uma lista e um inteiro (não negativo)
              -> [a] -- ^ resultado: uma lista
deslocae [] _ = []
deslocae (h:t) a | a == 0 = t ++ [h]
                 | otherwise = (deslocae (t ++ [h]) (a-1))

-- Exercício 4:

{-| Dada uma matriz l, a função ´trocalinhas´ troca a primeira linha com a última.

A função poderia ser definida da seguinte forma:

@
trocalinhas :: [[a]] -> [[a]]
trocalinhas l = [(last l)] ++ tail(init l) ++ [(head l)]
@ 

== Exemplos de utilização:

>>> trocalinhas [[1,2],[3,4],[5,6],[7,8]]
[[7,8],[3,4],[5,6],[1,2]]

-}

trocalinhas :: [[a]] -- ^ argumento: uma matriz
                   -> [[a]] -- ^ resultado: uma matriz
trocalinhas l = [(last l)] ++ tail(init l) ++ [(head l)]


-- Exercício 5:

{-| Dada uma matriz l, a função ´trocacolunas´ troca a primeira coluna com a última.

A função poderia ser definida da seguinte forma:

@
trocacolunas :: [[a]] -> [[a]]
trocacolunas [] = []
trocacolunas (h:t) = ([(last h)] ++ init (tail h) ++ (init h)) : trocacolunas t
@ 

== Exemplos de utilização:

>>> trocacolunas [[1,2],[3,4],[5,6],[7,8]]
[[2,1],[4,3],[6,5],[8,7]]

-}

trocacolunas :: [[a]] -- ^ argumento: uma matriz
                   -> [[a]] -- ^ resultado: uma matriz
trocacolunas [] = []
trocacolunas (h:t) = ([(last h)] ++ init (tail h) ++ (init h)) : trocacolunas t

-- Exercício 6:
{-| 
A função ´posicao'´ é uma função auxiliar da função ´posicao´ que também recebe por quantos elementos já passou.
Quando verifica que o elemento não é o pretendido, chama-se a si própria, mas incrementando um acumulador.

A função poderia ser definida da seguinte forma:

@
posicao' :: Eq a => Int -> a -> [a] -> Int
posicao' _ _ [] = -1
posicao' i a (h:t) | h == a = i
                   | otherwise = posicao' (i+1) a t
@
-}

posicao' :: Eq a => Int -> a -> [a] -> Int
posicao' _ _ [] = -1
posicao' i a (h:t) | h == a = i
                   | otherwise = posicao' (i+1) a t

{-| A função ´posicao´, com o auxílio da função ´posicao'´, procura a posição de um elemento numa dada lista retornando apenas a primeira ocorrência.
Caso o elemento não ocorra na lista a função devolve o número inteiro -1.

A função poderia ser definida da seguinte forma:

@
posicao :: Eq a => a -> [a] -> Int
posicao = posicao' 0
@ 

== Exemplos de utilização:

>>> posicao 2 [3,4,5]
-1
>>> posicao 2 [3,4,5,2]
3
>>> posicao 2 [32,2,4,5,2]
1
-}

posicao :: Eq a => a -> [a] -- ^ argumentos: um elemento da lista e a lista
                                    -> Int -- ^ resultado: posição do elemento
posicao = posicao' 0



-- Exercício 7:

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

-- Exercício 8:

{-|
A função ´encM'´ é uma função auxiliar da função ´encM´ que também recebe por quantos elementos já passou.
Quando verifica que o elemento não é o pretendido, chama-se a si própria, mas incrementando um acumulador.

A função poderia ser definida da seguinte forma:

@
encM' :: Eq a => (Int,Int) -> a -> [[a]] -> (Int,Int)
encM' _ _ [] = (-1,-1)
encM' (i,j) a (h:t) | posicao a h == -1 = encM' (i+1,0) a t
                    | otherwise = (i,j+ posicao a h)
@

-}

encM' :: Eq a => (Int,Int) -> a -> [[a]] -> (Int,Int)
encM' _ _ [] = (-1,-1)
encM' (i,j) a (h:t) | posicao a h == -1 = encM' (i+1,0) a t
                    | otherwise = (i,j+ posicao a h)

{-| A função recursiva ´encM´, com o auxílio da função ´encM'´, irá encontrar a primeira ocorrência de um dado elemento numa matriz. 

A função poderia ser definida da seguinte forma:

@
encM :: Eq a => a -> [[a]] -> (Int,Int)
encM = encM' (0,0)
@ 

== Exemplos de utilização:

>>> encM 2 [[1,2],[3,4],[5,6],[7,8]]
(0,1)

>>> encM 5 [[1,2],[3,4],[5,6],[7,8]]
(2,0)

-}

encM :: Eq a => a -> [[a]] -- ^ argumentos: elemento, uma matriz
                         -> (Int,Int) -- ^ resultado: posição do elemento
encM = encM' (0,0)

-- Exercício 9:

{-| A função recursiva ´subsM´ substitui um elemento de uma posição fornecida numa matriz, por outro valor.

A função poderia ser definida da seguinte forma:

@
subsM :: Eq a => (Int,Int) -> a -> [[a]] -> [[a]]
subsM _ _ [] = []
subsM (i,j) a (h:t) | i>length (h:t) = (h:t)
                    | i == 0 = (subs h j a:t)
                    | otherwise = h:(subsM (i-1,j) a t)
@ 

== Exemplos de utilização:

>>> subsM (4,1) 2 [[1,2],[3,4],[5,6],[7,8]]
[[1,2],[3,4],[5,6],[7,8]]

>>> subsM (3,1) 2 [[1,2],[3,4],[5,6],[7,8]]
[[1,2],[3,4],[5,6],[2,8]]

>>> subsM (2,0) 2 [[1,2],[3,4],[5,6],[7,8]]
[[1,2],[3,4],[2,6],[7,8]]

-}

subsM :: Eq a => (Int,Int) -> a -> [[a]] -- ^ argumentos: posição da substituição, valor dado e uma matriz
                                     -> [[a]] -- ^ resultado: uma matriz
subsM _ _ [] = []
subsM (i,j) a (h:t) | i>length (h:t) = (h:t)
                    | i == 0 = (subs h j a:t)
                    | otherwise = h:(subsM (i-1,j) a t)