module Tarefa5_2022li1g007_Spec where

import LI12223
import Tarefa5_2022li1g007
import Test.HUnit

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test ["teste1" ~: b1  ~=? a1,
                                              "teste2" ~: b2  ~=? a2,
                                              "teste3" ~: b3  ~=? a3,
                                              "teste4" ~: b4  ~=? a4,
                                              "teste5" ~: b5  ~=? a5,
                                              "teste6" ~: b6  ~=? a6]

--deslizaJogo
a1 = deslizaJogo 2 (Jogo (Jogador (2,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])]))
b1 = Jogo (Jogador (2,3)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum])])

a2 = deslizaJogo 35 (Jogo (Jogador (1,3)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b2 = Jogo (Jogador (1,4)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])])

a3 = deslizaJogo 1 (Jogo (Jogador (2,0)) (Mapa 3 [(Relva, [Arvore,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])]))
b3 = Jogo (Jogador (2,1)) (Mapa 3 [(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum])])

a4 = deslizaJogo 10 (Jogo (Jogador (1,1)) (Mapa 4 [(Rio (-1) ,[Nenhum,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])]))
b4 = Jogo (Jogador (1,2)) (Mapa 4 [(Estrada (-1),[Carro,Carro,Carro,Nenhum]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

a5 = deslizaJogo 90 (Jogo (Jogador (1,2)) (Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro]),(Rio 2,[Nenhum,Tronco,Nenhum,Tronco])]))
b5 = Jogo (Jogador (1,3)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro])])

a6 = deslizaJogo 55 (Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b6 = Jogo (Jogador (2,1)) (Mapa 4 [(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum])])