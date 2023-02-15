{- |
Module      : Tarefa4_2022li1g007
Description : Determinar se o jogo terminou
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa4_2022li1g007 where

import LI12223

{-| A função 'jogoTerminou' determina se o jogador perdeu o jogo, caso tal tenha sucedido então a função retorna True.

Para ser possível verificar se o jogador perdeu, tem que se verificar uma destas situações (com o auxílio de algumas funções):

- Caso o jogador se encontre fora do mapa: 'foraMapa';

- Caso o jogador se encontre na água, mas não esteja num Tronco: 'afogamento';

- Caso o jogador se encontre debaixo de um carro: 'atropelamento'.

A função poderia ser definida da seguinte forma:

@
jogoTerminou :: Jogo -> Bool
jogoTerminou jogo = foraMapa jogo || afogamento jogo || atropelamento jogo 
@

== Exemplos de utilização:

>>> jogoTerminou (Jogo (Jogador (0,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False

>>> jogoTerminou (Jogo (Jogador (5,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> jogoTerminou (Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True
-}

jogoTerminou :: Jogo         -- ^ argumento: Jogo
                     -> Bool -- ^ resultado: verifica se o jogador perdeu o jogo
jogoTerminou jogo = foraMapa jogo || afogamento jogo || atropelamento jogo 

{-| A função 'foraMapa' averigua se a personagem se encontra fora do mapa indicado.

Pode-se verificar que a personagem está fora do mapa quando:

- A sua abcissa é superior ou igual à largura do mapa, isto é, a personagem está fora do terreno;

- A sua ordenada é superior ou igual ao número de terrenos, ou seja, a personagem não se encontra em nenhum terreno existente;

- A sua abcissa ou ordenada são inferiores a 0.

A função poderia ser definida da seguinte forma:

@
foraMapa :: Jogo -> Bool
foraMapa (Jogo (Jogador (x,y)) (Mapa l lT)) = x >= l || y >= length lT || x < 0 || y < 0
@

== Exemplos de utilização:

>>> foraMapa (Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False

>>> foraMapa (Jogo (Jogador (5,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> foraMapa (Jogo (Jogador (0,3)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True
-}

foraMapa :: Jogo         -- ^ argumento: Jogo
                 -> Bool -- ^ resultado: True caso a personagem esteja fora do mapa, False caso contrário
foraMapa (Jogo (Jogador (x,y)) (Mapa l lT)) = x >= l || y >= length lT || x < 0 || y < 0

{-| A função 'afogamento' averigua se o jogador se encontre num Rio e não se situe em cima de um Tronco.

Para tal com o auxílio da função 'terrenoString', a 'afogamento' verifica se o jogador se encontra num Rio, 
se tal acontecer, caso o jogador não se encontre num Tronco, 
então o jogador afogou-se e retorna True. Caso contrário, retorna False.

A função poderia ser definida da seguinte forma:

@
afogamento :: Jogo -> Bool
afogamento (Jogo (Jogador (x,y)) (Mapa l lT))
   | (terrenoString lT) !! y == "Rio" && (snd(lT !! y) !! x) == Nenhum = True
   | otherwise = False
@

== Exemplos de utilização:

>>> afogamento (Jogo (Jogador (0,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> afogamento (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False

>>> afogamento (Jogo (Jogador (1,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False
-}

afogamento :: Jogo       -- ^ argumento: Jogo
                 -> Bool -- ^ resultado: True caso a personagem se encontre na água e não esteja num Tronco, False caso contrário
afogamento (Jogo (Jogador (x,y)) (Mapa l lT))
   | (terrenoString lT) !! y == "Rio" && (snd(lT !! y) !! x) == Nenhum = True
   | otherwise = False

{-| A função 'atropelamento' recebe um Jogo e averigua se a personagem se encontra "debaixo" de um Carro.

Esta verifica se a posição do jogador coincide com a posição de um Carro, caso
tal aconteça, o jogador foi atropelado e a função retorna True. Caso contrário, retorna False.

A função poderia ser definida da seguinte forma:

@
atropelamento :: Jogo -> Bool
atropelamento (Jogo (Jogador (x,y)) (Mapa l lT))
   |(terrenoString lT) !! y == "Estrada" && (snd(lT !! y) !! x) == Carro = True
   |otherwise = False
@

== Exemplos:

>>> atropelamento (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum,Arvore,Arvore]),(Estrada (-2), [Nenhum,Carro,Nenhum]), (Estrada 1, [Carro,Carro,Nenhum])]))
True

>>> atropelamento (Jogo (Jogador (0,1)) (Mapa 3 [(Relva, [Nenhum,Arvore,Arvore]),(Estrada (-2), [Nenhum,Carro,Nenhum]), (Estrada 1, [Carro,Carro,Nenhum])]))
False
-}

atropelamento :: Jogo         -- ^ argumento: Jogo 
                     -> Bool  -- ^ resultado: True caso a personagem se encontre na mesma posição que o carro, caso contrário, False
atropelamento (Jogo (Jogador (x,y)) (Mapa l lT))
   | (terrenoString lT) !! y == "Estrada" && (snd(lT !! y) !! x) == Carro = True
   | otherwise = False

{-| A função 'terrenoString' é uma função auxiliar da função 'afogamento' e 'atropelamento'.
Esta recebe uma lista de Terrenos e Obstáculos e devolve uma lista dos nomes dos Terrenos em String.

A função poderia ser definida da seguinte forma:

@
terrenoString :: [(Terreno,[Obstaculo])] -> [String]
terrenoString [] = []
terrenoString ((e,l):t) = case e of Estrada _ -> "Estrada":terrenoString t
                                    Rio _     -> "Rio":terrenoString t
                                    Relva     -> "Relva":terrenoString t
@

== Exemplos:

>>> terrenoString [(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Relva,[Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum])]
["Estrada","Estrada","Relva","Rio"]
-}

terrenoString :: [(Terreno,[Obstaculo])]            -- ^ argumento: Lista de terrenos
                                        -> [String] -- ^ resultado: Lista de terrenos em formato String
terrenoString [] = []
terrenoString ((e,l):t) = case e of Estrada _ -> "Estrada":terrenoString t
                                    Rio _     -> "Rio"    :terrenoString t
                                    Relva     -> "Relva"  :terrenoString t