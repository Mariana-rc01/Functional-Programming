{- |
Module      : Tarefa5_2022li1g007
Description : Deslize do Jogo
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g007 where

import LI12223
import Tarefa2_2022li1g007

{-| A função 'deslizaJogo' tem como objetivo dar a sensação que o mapa está a deslizar.

De modo que, com o auxílio da função 'estendeMapa', a função 'deslizaJogo' faz com que a última linha desapareça e que uma nova linha
seja adicionada ao topo do mapa (sendo esta criada pela função 'estendeMapa').

É importante relembrar que a ordenada do Jogador é aumentada, com efeito de que o jogador não avançou.


A função poderia ser definida da seguinte forma:

@
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) mapa) = Jogo (Jogador (x,y+1)) (Mapa l (init lT))
                                           where Mapa l lT = estendeMapa mapa seed
@

== Exemplos de utilização:

>>> deslizaJogo 2 (Jogo (Jogador (2,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])]))
Jogo (Jogador (2,3)) (Mapa 4 [(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum])])

>>> deslizaJogo 75 (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
Jogo (Jogador (1,1)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])])
-}   

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) mapa) = Jogo (Jogador (x,y+1)) (Mapa l (init lT))
                                           where Mapa l lT = estendeMapa mapa seed