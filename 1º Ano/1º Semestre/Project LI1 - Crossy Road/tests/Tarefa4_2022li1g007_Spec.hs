module Tarefa4_2022li1g007_Spec where

import LI12223
import Tarefa4_2022li1g007
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [
    "jogoTerminou jogador fora do mapa T" ~: True                                ~=?  a1  ,
    "jogoTerminou jogador fora do mapa F" ~: False                               ~=?  b1  ,

    "jogoTerminou jogador afogado T"      ~: True                                ~=?  a2  ,
    "jogoTerminou jogador afogado F"      ~: False                               ~=?  b2  ,

    "jogoTerminou jogador atropelado T"   ~: True                                ~=?  a3  ,
    "jogoTerminou jogador atropelado F"   ~: False                               ~=?  b3  ,

    "foraMapa T"                          ~: True                                ~=?  a4  ,
    "foraMapa F"                          ~: False                               ~=?  b4  ,

    "afogamento T"                        ~: True                                ~=?  a5  ,
    "afogamento F"                        ~: False                               ~=?  b5  ,

    "atropelamento T"                     ~: True                                ~=?  a6  ,
    "atropelamento F"                     ~: False                               ~=?  b6  ,

    "terrenoString T"                     ~: ["Estrada","Estrada","Relva","Rio"] ~=?  a7  ,
    "terrenoString F"                     ~: ["Relva","Estrada","Estrada"]       ~=?  b7 

    ]

a1 = jogoTerminou (Jogo (Jogador (5,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b1 = jogoTerminou (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

a2 = jogoTerminou (Jogo (Jogador (0,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b2 = jogoTerminou (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

a3 = jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1, [Carro,Nenhum]),(Rio 2, [Nenhum,Tronco])]))
b3 = jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 1, [Carro,Nenhum]),(Relva, [Nenhum,Arvore])]))

a4 = foraMapa (Jogo (Jogador (5,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b4 = foraMapa (Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))


a5 = afogamento (Jogo (Jogador (0,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b5 = afogamento (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

a6 = atropelamento (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum,Arvore,Arvore]),(Estrada (-2), [Nenhum,Carro,Nenhum]), (Estrada 1, [Carro,Carro,Nenhum])]))
b6 = atropelamento (Jogo (Jogador (0,0)) (Mapa 3 [(Relva, [Nenhum,Arvore,Arvore]),(Estrada (-2), [Nenhum,Carro,Nenhum]), (Estrada 1, [Carro,Carro,Nenhum])]))

a7 = terrenoString [(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Relva,[Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum])]
b7 = terrenoString [(Relva, [Nenhum,Arvore,Arvore]),(Estrada (-2), [Nenhum,Carro,Nenhum]), (Estrada 1, [Carro,Carro,Nenhum])]