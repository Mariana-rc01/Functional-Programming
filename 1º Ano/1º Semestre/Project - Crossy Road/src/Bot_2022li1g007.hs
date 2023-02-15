{- |
Module      : Bot_2022li1g007
Description : Bot do Jogo
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module Bot_2022li1g007 where

import LI12223
import Tarefa3_2022li1g007

{-| A função 'bot' recebe um Jogo e, com o auxílio da 'moveBot', verifica para onde o Jogador se pode mover e move-se consoante o resultado obtido.
Caso não seja possível efetuar nenhum movimento, então o Jogo mantém-se o mesmo.

A função poderia ser definida da seguinte forma:

@
bot :: Jogo -> Jogo
bot jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l lT))
   | y > 0             && moveBot (Jogo (Jogador (x,y-1)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Cima)
   | x > 0             && moveBot (Jogo (Jogador (x-1,y)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Esquerda)
   | x < l-1           && moveBot (Jogo (Jogador (x+1,y)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Direita)
   | y < length lT - 1 && moveBot (Jogo (Jogador (x,y+1)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Baixo)
   | otherwise = animaJogo jogo Parado
@

== Exemplos de utilização:

>>> bot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
Jogo (Jogador (2,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> bot (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
Jogo (Jogador (1,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> bot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
Jogo (Jogador (2,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> bot (Jogo (Jogador (2,3)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Carro])]))
Jogo (Jogador (2,3)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco]),(Estrada (-1),[Carro,Nenhum,Carro,Carro])])
-}


bot :: Jogo         -- ^ argumento: Jogo
            -> Jogo -- ^ resultado: Jogo
bot jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l lT))
   | y > 0             && moveBot (Jogo (Jogador (x,y-1)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Cima)
   | x > 0             && moveBot (Jogo (Jogador (x-1,y)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Esquerda)
   | x < l-1           && moveBot (Jogo (Jogador (x+1,y)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Direita)
   | y < length lT - 1 && moveBot (Jogo (Jogador (x,y+1)) (Mapa l (moveObstaculoAux lT))) = animaJogo jogo (Move Baixo)
   | otherwise = animaJogo jogo Parado


{- A função 'moveBot' recebe um Jogo e verifica se o Jogador se encontra num obstáculo que lhe permita não perder o Jogo.
De modo que caso se encontre numa Estrada ou numa Relva, tem que estar em Nenhum, caso esteja num Rio, precisa de se encontrar num Tronco.
Ela verifica se não há nenhum obstáculo em frente ao jogador e caso não haja faz-o avançar.

A função poderia ser definida da seguinte forma:

@
moveBot (Jogo (Jogador (x,y)) (Mapa l lT)) 
      | (passaString (fst(lT !! y))) == "Estrada" || (passaString (fst(lT !! y))) == "Relva" = obstaculo == Nenhum
      | (passaString (fst(lT !! y))) == "Rio"                                                = obstaculo == Tronco
   where passaString :: Terreno -> String
         passaString t = case t of Estrada _ -> "Estrada"
                                   Rio _     -> "Rio"    
                                   Relva     -> "Relva"
         obstaculo = ((snd(lT !! y)) !! x)
@

== Exemplos de utilização:

>>> moveBot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> moveBot (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> moveBot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> moveBot (Jogo (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False
-}

moveBot :: Jogo         -- ^ argumento: Jogo
                -> Bool -- ^ resultado: True caso possa se mover para lá, caso contrário False
moveBot (Jogo (Jogador (x,y)) (Mapa l lT)) 
      | (passaString (fst(lT !! y))) == "Estrada" || (passaString (fst(lT !! y))) == "Relva" = obstaculo == Nenhum
      | (passaString (fst(lT !! y))) == "Rio"                                                = obstaculo == Tronco
   where passaString :: Terreno -> String
         passaString t = case t of Estrada _ -> "Estrada"
                                   Rio _     -> "Rio"    
                                   Relva     -> "Relva"
         obstaculo = ((snd(lT !! y)) !! x)
