module Bot_2022li1g007_Spec where

import LI12223
import Bot_2022li1g007
import Test.HUnit

testsTB :: Test
testsTB = TestLabel "Testes do Bot" $ test [

    "bot quando pode se pode Mover Cima"            ~: b1  ~=?  a1,

    "bot quando irá ter Rio sem Tronco à frente"    ~: b2  ~=?  a2,

    "bot quando tem uma Arvore à frente"            ~: b3  ~=?  a3,

    "bot quando não pode realizar nenhum movimento" ~: b4  ~=?  a4,

    "bot quando vai ser atropelado por um Carro"    ~: b5  ~=?  a5
    ]

a1 = bot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b1 = Jogo (Jogador (2,1)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

a2 = bot (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b2 = Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

a3 = bot (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
b3 = Jogo (Jogador (0,2)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

a4 = bot (Jogo (Jogador (2,3)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Carro])]))
b4 = Jogo (Jogador (1,3)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Carro])])

a5 = bot (Jogo (Jogador (2,3)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Carro])]))
b5 = Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Nenhum,Carro,Carro])])