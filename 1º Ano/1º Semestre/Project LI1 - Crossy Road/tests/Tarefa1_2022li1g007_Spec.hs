module Tarefa1_2022li1g007_Spec where

import LI12223
import Tarefa1_2022li1g007
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [

    "mapaValido de um mapa que respeite todas as condições"               ~: True  ~=?  mapaValido mapa8,
    "mapaValido onde uma lista de obstáculos é diferente da largura"      ~: False ~=?  mapaValido map7,
    "mapaValido quando os obstáculos não estão no terreno correto"        ~: False ~=?  mapaValido map1,
    "mapaValido quando dois rios seguidos não têm velocidades contrárias" ~: False ~=?  mapaValido map2,
    "mapaValido quando existe um carro com 4 unidades"                    ~: False ~=?  mapaValido map3,
    "mapaValido quando existe um tronco com 6 unidades"                   ~: False ~=?  mapaValido map31,
    "mapaValido quando não existe Nenhum numa lista de obstáculos"        ~: False ~=?  mapaValido map4,
    "mapaValido quando existem 5 Rios seguidos"                           ~: False ~=?  mapaValido map6,
    "mapaValido quando existem 6 Estradas seguidas"                       ~: False ~=?  mapaValido map9,
    "mapaValido quando existem 6 Relvas seguidas"                         ~: False ~=?  mapaValido map10,

    "validaObst T"                                                        ~: True  ~=?  validaObst mapa1,
    "validaObst F"                                                        ~: False ~=?  validaObst map1,

    "validaRios T"                                                        ~: True  ~=?  validaRios mapa2,
    "validaRios F"                                                        ~: False ~=?  validaRios map2,

    "validaComp T"                                                        ~: True  ~=?  validaComp mapa3,
    "validaComp F"                                                        ~: False ~=?  validaComp map3,

    "validaComp1 T"                                                       ~: True  ~=?  validaComp1 mapa31,
    "validaComp1 F"                                                       ~: False ~=?  validaComp1 map31,

    "validaComp2 T"                                                       ~: True  ~=?  validaComp2 mapa32,
    "validaComp2 F"                                                       ~: False ~=?  validaComp2 map32,

    "validaCaminho T"                                                     ~: True  ~=?  validaCaminho mapa4,
    "validaCaminho F"                                                     ~: False ~=?  validaCaminho map4,

    "validaTamanho T"                                                     ~: True  ~=?  validaTamanho mapa5,
    "validaTamanho F"                                                     ~: False ~=?  validaTamanho map5,

    "validaSeq T"                                                         ~: True  ~=?  validaSeq mapa6,
    "validaSeq F"                                                         ~: False ~=?  validaSeq map6
    ]

mapa1 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
map1  = (Mapa 5 [(Relva, [Carro, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])


mapa2 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map2  = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio 2, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])


mapa3 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Tronco, Tronco]),(Estrada 1, [Nenhum, Nenhum, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map3  = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Tronco, Tronco]),(Estrada 1, [Nenhum, Carro, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

mapa31= (Mapa 4 [(Relva, [Nenhum, Arvore, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Tronco]),(Rio (-2), [Tronco, Nenhum, Tronco, Nenhum])])
map31 = (Mapa 6 [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum,Tronco])])

mapa32= (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada 1, [Carro, Carro, Nenhum, Nenhum, Carro])])
map32 = (Mapa 7 [(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])])

mapa4 = (Mapa 5 [(Relva, [Arvore, Arvore, Arvore, Nenhum, Arvore]),(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map4  = (Mapa 5 [(Relva, [Arvore, Arvore, Arvore, Arvore, Arvore]),(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

mapa5 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada 2, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map5  = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada 2, [Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

mapa6 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map6  = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio 2, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

mapa7 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada 3, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
map7  = (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Arvore]),(Estrada 3, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

mapa8 = (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Carro,Nenhum]),(Estrada (-1),[Nenhum,Carro,Carro,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum])])

map9  = (Mapa 2 [(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada 1,[Nenhum,Carro]),(Estrada (-1),[Nenhum,Carro]),(Estrada 1,[Nenhum,Carro])])
map10 = (Mapa 2 [(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum])])
