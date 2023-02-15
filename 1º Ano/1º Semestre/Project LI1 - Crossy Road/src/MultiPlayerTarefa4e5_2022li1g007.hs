{- |
Module      : MultiPlayerTarefa4e5_2022li1g007
Description : Modo multiplayer
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module MultiPlayerTarefa4e5_2022li1g007 where

import LI12223
import Tarefa2_2022li1g007
import Tarefa4_2022li1g007

{-| A função 'jogoTerminouM' determina se o jogo terminou, ou seja, se um dos jogadores perdeu, caso tal tenha sucedido então a função retorna True.

Para ser possível verificar se o jogador perdeu, é usada a função 'jogoTerminou'.

A função poderia ser definida da seguinte forma:

@
jogoTerminouM :: JogoM -> Bool
jogoTerminouM (JogoM jogador1 jogador2 mapa) = jogoTerminou (Jogo jogador1 mapa) || jogoTerminou (Jogo jogador2 mapa)
@

== Exemplos de utilização:

>>> jogoTerminouM (JogoM (Jogador (0,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> jogoTerminouM (JogoM (Jogador (0,0)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False
-}

jogoTerminouM :: JogoM -> Bool
jogoTerminouM (JogoM jogador1 jogador2 mapa) = jogoTerminou (Jogo jogador1 mapa) || jogoTerminou (Jogo jogador2 mapa)

{-| A função 'ambosPerderamM' determina se o jogo terminou quando os dois jogadores perdem, caso tal tenha sucedido então a função retorna True.

Para ser possível verificar se o jogador perdeu, é usada a função 'jogoTerminou'.

A função poderia ser definida da seguinte forma:

@
ambosPerderamM :: JogoM -> Bool
ambosPerderamM (JogoM jogador1 jogador2 mapa) = jogoTerminou (Jogo jogador1 mapa) && jogoTerminou (Jogo jogador2 mapa)
@

== Exemplos de utilização:

>>> ambosPerderamM (JogoM (Jogador (0,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False

>>> ambosPerderamM (JogoM (Jogador (0,0)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
True

>>> ambosPerderamM (JogoM (Jogador (1,0)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False

>>> ambosPerderamM (JogoM (Jogador (1,0)) (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
False
-}

ambosPerderamM :: JogoM -> Bool
ambosPerderamM (JogoM jogador1 jogador2 mapa) = jogoTerminou (Jogo jogador1 mapa) && jogoTerminou (Jogo jogador2 mapa)


{-| A função 'deslizaJogoM' tem como objetivo dar a sensação que o mapa está a deslizar.

De modo que, com o auxílio da função 'estendeMapa', a função 'deslizaJogoM' faz com que a última linha desapareça e que uma nova linha
seja adicionada ao topo do mapa (sendo esta criada pela função 'estendeMapa').

É importante relembrar que as ordenadas dos Jogadores é aumentada, com efeito de que os jogadores não avançaram.


A função poderia ser definida da seguinte forma:

@
deslizaJogoM :: Int -> JogoM -> JogoM
deslizaJogoM seed (JogoM (Jogador (x1,y1)) (Jogador (x2,y2)) mapa) = JogoM (Jogador (x1,y1+1)) (Jogador (x2,y2+1)) (Mapa l (init lT))
                                           where Mapa l lT = estendeMapa mapa seed
@

== Exemplos de utilização:

>>> deslizaJogoM 50 (JogoM (Jogador (0,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
JogoM (Jogador (0,1)) (Jogador (1,1)) (Mapa 4 [(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])])

>>> deslizaJogoM 87 (JogoM (Jogador (2,3)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
JogoM (Jogador (2,4)) (Jogador (1,1)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])])
-}   

deslizaJogoM :: Int -> JogoM -> JogoM
deslizaJogoM seed (JogoM (Jogador (x1,y1)) (Jogador (x2,y2)) mapa) = JogoM (Jogador (x1,y1+1)) (Jogador (x2,y2+1)) (Mapa l (init lT))
                                           where Mapa l lT = estendeMapa mapa seed