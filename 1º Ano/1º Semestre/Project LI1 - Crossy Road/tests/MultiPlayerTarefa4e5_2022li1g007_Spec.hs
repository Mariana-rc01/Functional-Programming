module MultiPlayerTarefa4e5_2022li1g007_Spec where

import LI12223
import MultiPlayerTarefa4e5_2022li1g007
import Test.HUnit

testsTM4 :: Test
testsTM4 = TestLabel "Testes Tarefa MultiPlayer 4" $ test [

    "jogoTerminouM um jogador fora do mapa"     ~: True  ~=?  a1,
    "jogoTerminouM nenhum jogador fora do mapa" ~: False ~=?  b1,

    "jogoTerminouM um jogador afogado"          ~: True  ~=?  a2,
    "jogoTerminouM nenhum jogador afogado"      ~: False ~=?  b2,

    "jogoTerminouM um jogador atropelado"       ~: True  ~=?  a3,
    "jogoTerminouM nenhum jogador atropelado"   ~: False ~=?  b3
    ]

a1 = jogoTerminouM (JogoM (Jogador (5,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b1 = jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (1,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

a2 = jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (0,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b2 = jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (2,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

a3 = jogoTerminouM (JogoM (Jogador (0,0)) (Jogador (1,1)) (Mapa 2 [(Estrada 1, [Carro,Nenhum]),(Rio 2, [Nenhum,Tronco])]))
b3 = jogoTerminouM (JogoM (Jogador (1,0)) (Jogador (0,1)) (Mapa 2 [(Estrada 1, [Carro,Nenhum]),(Relva, [Nenhum,Arvore])]))


testsTM5 :: Test
testsTM5 = TestLabel "Testes Tarefa MultiPlayer 5" $ test ["teste4" ~: b4  ~=? a4,
                                                           "teste5" ~: b5  ~=? a5,
                                                           "teste6" ~: b6  ~=? a6,
                                                           "teste7" ~: b7  ~=? a7,
                                                           "teste8" ~: b8  ~=? a8,
                                                           "teste9" ~: b9  ~=? a9]

--deslizaJogo
a4 = deslizaJogoM 2 (JogoM (Jogador (2,2)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])]))
b4 = JogoM (Jogador (2,3)) (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum])])

a5 = deslizaJogoM 35 (JogoM (Jogador (1,3)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b5 = JogoM (Jogador (1,4)) (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])])

a6 = deslizaJogoM 1 (JogoM (Jogador (2,0)) (Jogador (0,1)) (Mapa 3 [(Relva, [Arvore,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])]))
b6 = JogoM (Jogador (2,1)) (Jogador (0,2)) (Mapa 3 [(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Nenhum])])

a7 = deslizaJogoM 10 (JogoM (Jogador (1,1)) (Jogador (2,2)) (Mapa 4 [(Rio (-1) ,[Nenhum,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])]))
b7 = JogoM (Jogador (1,2)) (Jogador (2,3)) (Mapa 4 [(Estrada (-1),[Carro,Carro,Carro,Nenhum]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

a8 = deslizaJogoM 90 (JogoM (Jogador (1,2)) (Jogador (2,0)) (Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro]),(Rio 2,[Nenhum,Tronco,Nenhum,Tronco])]))
b8 = JogoM (Jogador (1,3)) (Jogador (2,1)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro])])

a9 = deslizaJogoM 55 (JogoM (Jogador (2,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b9 = JogoM (Jogador (2,1)) (Jogador (1,1)) (Mapa 4 [(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum])])
