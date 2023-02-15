module MultiPlayerTarefa3_2022li1g007_Spec where

import LI12223
import MultiPlayerTarefa3_2022li1g007
import Test.HUnit

testsTM3 :: Test
testsTM3 = TestLabel "Testes Tarefa MultiPlayer 3" $ test [

    "animaJogo quando as personagens se encontram Parados"                                         ~: parado            ~=? animaJogoM jogoParado [Parado,Parado],
    "animaJogo quando a personagem se encontra Parados num Tronco e Move Cima"                     ~: paradoTronco      ~=? animaJogoM jogoParadoTronco [Parado,Move Cima],
    
    "animaJogo quando uma personagem se encontra num Tronco e Move Cima com Arvore nessa posição"  ~: cimaTroncoArvore  ~=? animaJogoM jogoCimaTA [Move Cima,Parado],
    "animaJogo quando uma personagem se encontra num Tronco e Move Baixo sem Arvore nessa posição" ~: baixoTroncoArvore ~=? animaJogoM jogoBaixoTA [Move Baixo,Move Esquerda],
    "animaJogo quando uma personagem se encontra num Tronco e Move Esquerda"                       ~: esquerdaTronco    ~=? animaJogoM jogoT [Move Esquerda,Move Direita],
    "animaJogo quando uma personagem se encontra num Tronco e Move Direita"                        ~: direitaTronco     ~=? animaJogoM jogoT [Move Direita,Move Baixo],

    "animaJogo quando uma personagem se encontra no topo do mapa e Move Cima"                      ~: topo               ~=? animaJogoM jogoTopo [Move Cima,Move Cima],
    "animaJogo quando uma personagem se encontra no final do mapa e Move Baixo"                    ~: final              ~=? animaJogoM jogoBaixo [Move Baixo,Move Esquerda],
    "animaJogo quando uma personagem se encontra no limite esquerdo do mapa e Move Esquerda"       ~: limiteEsq          ~=? animaJogoM jogoEsquerda [Move Esquerda,Move Direita],
    "animaJogo quando uma personagem se encontra no limite direito do mapa e Move Direita"         ~: limiteDir          ~=? animaJogoM jogoDireita [Move Direita,Parado],

    "moveObstaculo da lista1 com um Jogador na mesma posição que um carro"                         ~: lista11            ~=? moveObstaculoM lista1 [Parado,Move Cima] (Jogador (1,2)) (Jogador (1,0)),
    "moveObstaculo da lista2"                                                                      ~: lista22            ~=? moveObstaculoM lista2 [Move Baixo,Move Esquerda] (Jogador (1,0)) (Jogador (2,0))]


-- Jogos para testar animaJogoM e moveJogadorM:

jogoParado       = (JogoM (Jogador (2,2)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
jogoParadoTronco = (JogoM (Jogador (3,0)) (Jogador (1,2)) (Mapa 4 [(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

mapa = (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

parado       = JogoM (Jogador (2,2)) (Jogador (1,2)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
paradoTronco = JogoM (Jogador (2,0)) (Jogador (1,1)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])


jogoCimaTA  = (JogoM (Jogador (1,1)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoBaixoTA = (JogoM (Jogador (1,0)) (Jogador (2,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
jogoT       = (JogoM (Jogador (2,1)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))

cimaTroncoArvore  = JogoM (Jogador (0,1)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
baixoTroncoArvore = JogoM (Jogador (2,0)) (Jogador (1,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])
esquerdaTronco    = JogoM (Jogador (0,1)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
direitaTronco     = JogoM (Jogador (2,1)) (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])


jogoTopo     = (JogoM (Jogador (1,0)) (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoBaixo    = (JogoM (Jogador (1,2)) (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoEsquerda = (JogoM (Jogador (0,1)) (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoDireita  = (JogoM (Jogador (3,0)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))

topo      = JogoM (Jogador (1,0)) (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
final     = JogoM (Jogador (1,2)) (Jogador (-1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
limiteEsq = JogoM (Jogador (0,1)) (Jogador (3,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
limiteDir = JogoM (Jogador (3,0)) (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])

-- lista de pares de terrenos e de listas de obstáculos para testar moveObstaculoM:

lista1  = [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum])]
lista2  = [(Rio 2,[Nenhum,Tronco,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]

lista11 = [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum])]
lista22 = [(Rio 2,[Tronco,Tronco,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]