module Tarefa3_2022li1g007_Spec where

import LI12223
import Tarefa3_2022li1g007
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [

    "animaJogo quando a personagem se encontra Parado"                                           ~: parado            ~=? animaJogo jogoParado Parado,
    "animaJogo quando a personagem se encontra Parado num Tronco"                                ~: paradoTronco      ~=? animaJogo jogoParadoTronco Parado,
    
    "animaJogo quando a personagem se encontra num Tronco e Move Cima com Arvore nessa posição"  ~: cimaTroncoArvore  ~=? animaJogo jogoCimaTA (Move Cima),
    "animaJogo quando a personagem se encontra num Tronco e Move Baixo sem Arvore nessa posição" ~: baixoTroncoArvore ~=? animaJogo jogoBaixoTA (Move Baixo),
    "animaJogo quando a personagem se encontra num Tronco e Move Esquerda"                       ~: esquerdaTronco    ~=? animaJogo jogoT (Move Esquerda),
    "animaJogo quando a personagem se encontra num Tronco e Move Direita"                        ~: direitaTronco     ~=? animaJogo jogoT (Move Direita),

    "animaJogo quando a personagem se encontra no topo do mapa e Move Cima"                      ~: topo               ~=? animaJogo jogoTopo (Move Cima),
    "animaJogo quando a personagem se encontra no final do mapa e Move Baixo"                    ~: final              ~=? animaJogo jogoBaixo (Move Baixo),
    "animaJogo quando a personagem se encontra no limite esquerdo do mapa e Move Esquerda"       ~: limiteEsq          ~=? animaJogo jogoEsquerda (Move Esquerda),
    "animaJogo quando a personagem se encontra no limite direito do mapa e Move Direita"         ~: limiteDir          ~=? animaJogo jogoDireita (Move Direita),

    "animaJogo quando a personagem Move Cima e tem uma Arvore à sua frente"                      ~: cimaArvore         ~=? animaJogo jogoCimaAr (Move Cima),
    "animaJogo quando a personagem Move Baixo e tem uma Arvore atrás"                            ~: baixoArvore        ~=? animaJogo jogoBaixoAr (Move Baixo),
    "animaJogo quando a personagem se encontra do lado Esquerdo de uma Arvore e Move Direita"    ~: esquerdaArvore     ~=? animaJogo jogoEsquerdaAr (Move Esquerda),
    "animaJogo quando a personagem se encontra do lado Direito de uma Arvore e Move Esquerda"    ~: direitaArvore      ~=? animaJogo jogoDireitaAr (Move Direita),

    "animaJogo com Move Cima"                                                                    ~: cima               ~=? animaJogo jogo (Move Cima),
    "animaJogo com Move Baixo"                                                                   ~: baixo              ~=? animaJogo jogo (Move Baixo),
    "animaJogo com Move Esquerda"                                                                ~: esquerda           ~=? animaJogo jogo (Move Esquerda),
    "animaJogo com Move Direita"                                                                 ~: direita            ~=? animaJogo jogo (Move Direita),

    "moveJogador quando a personagem se encontra Parado"                                         ~: Jogador (2,2)      ~=? moveJogador (Jogador (2,2)) mapa Parado,
    "moveJogador quando a personagem se encontra em cima de um Tronco e está Parado"             ~: Jogador (2,0)      ~=? moveJogador (Jogador (1,0)) mapa Parado,
    "moveJogador quando a personagem se encontra no topo do mapa e Move Cima"                    ~: Jogador (2,0)      ~=? moveJogador (Jogador (1,0)) mapa (Move Cima),

    "moveObstaculo da lista1 com Jogador na mesma posição que um carro"                          ~: lista11            ~=? moveObstaculo lista1 Parado (Jogador (1,2)),
    "moveObstaculo da lista2"                                                                    ~: lista22            ~=? moveObstaculo lista2 (Move Baixo) (Jogador (1,0))]


-- Jogos para testar animaJogo e moveJogador:

jogoParado       = (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
jogoParadoTronco = (Jogo (Jogador (3,0)) (Mapa 4 [(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))

mapa = (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

parado       = Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])
paradoTronco = Jogo (Jogador (2,0)) (Mapa 4 [(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])


jogoCimaTA  = (Jogo (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoBaixoTA = (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]))
jogoT       = (Jogo (Jogador (2,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))

cimaTroncoArvore  = Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
baixoTroncoArvore = Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])
esquerdaTronco    = Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
direitaTronco     = Jogo (Jogador (2,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])


jogoTopo     = (Jogo (Jogador (1,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoBaixo    = (Jogo (Jogador (1,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoEsquerda = (Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoDireita  = (Jogo (Jogador (3,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))

topo      = Jogo (Jogador (1,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
final     = Jogo (Jogador (1,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
limiteEsq = Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
limiteDir = Jogo (Jogador (3,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])


jogoCimaAr     = (Jogo (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoBaixoAr    = (Jogo (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoEsquerdaAr = (Jogo (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))
jogoDireitaAr  = (Jogo (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]))

cimaArvore     = Jogo (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
baixoArvore    = Jogo (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
esquerdaArvore = Jogo (Jogador (2,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])
direitaArvore  = Jogo (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])])


jogo = (Jogo (Jogador (1,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro])]))

cima     = Jogo (Jogador (1,0)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Carro,Nenhum])])
baixo    = Jogo (Jogador (1,2)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Carro,Nenhum])])
esquerda = Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Carro,Nenhum])])
direita  = Jogo (Jogador (2,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Carro,Nenhum])])


-- lista de pares de terrenos e de listas de obstáculos para testar moveObstaculo:

lista1  = [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum])]
lista2  = [(Rio 2,[Nenhum,Tronco,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]

lista11 = [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum])]
lista22 = [(Rio 2,[Tronco,Tronco,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]