module Tarefa2_2022li1g007_Spec where

import LI12223
import Tarefa2_2022li1g007
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [

        "Terrenos válidos após 4 Rios seguidos"           ~: [Estrada 0,Relva]       ~=? proximosTerrenosValidos mapaTVal1,
        "Terrenos válidos após 5 Estradas seguidas"       ~: [Rio 0,Relva]           ~=? proximosTerrenosValidos mapaTVal2,
        "Terrenos válidos após 5 Relvas seguidas"         ~: [Rio 0,Estrada 0]       ~=? proximosTerrenosValidos mapaTVal3,
        "Terrenos válidos sem qualquer restrição"         ~: [Rio 0,Estrada 0,Relva] ~=? proximosTerrenosValidos mapaTVal4,

        "Obstáculos válidos com 5 Troncos seguidos"       ~: [Nenhum]                ~=? proximosObstaculosValidos largura1 rio1,
        "Obstáculos válidos com 3 Carros seguidos"        ~: [Nenhum]                ~=? proximosObstaculosValidos largura1 estrada1,
        "Obstáculos válidos com l=length[Obstaculo]"      ~: []                      ~=? proximosObstaculosValidos largura2 estrada2,
        "Obstáculos válidos quando [Obstaculo]=[]"        ~: [Nenhum,Tronco]         ~=? proximosObstaculosValidos largura2 rio2,
        "Obstáculos válidos quando Nenhum não é elemento" ~: [Nenhum]                ~=? proximosObstaculosValidos largura2 relva,

        "Estende mapa após 4 Rios seguidos, seed 10"      ~: mapaTVal1'              ~=? estendeMapa mapaTVal1 10,
        "Estende mapa após 5 Estradas seguidas, seed 0"   ~: mapaTVal2'              ~=? estendeMapa mapaTVal2 0,
        "Estende mapa após 5 Relvas seguidas, seed 27"    ~: mapaTVal3'              ~=? estendeMapa mapaTVal3 27,
        "Estende mapa sem qualquer restrição, seed 75"    ~: mapaTVal4'              ~=? estendeMapa mapaTVal4 75
        ]


-- Mapas para testar a função proximosTerrenosValidos e para a função estendeMapa:

mapaTVal1 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco])])
mapaTVal2 = (Mapa 2 [(Estrada 1, [Carro,Nenhum]),(Estrada 1, [Nenhum,Carro]),(Estrada (-1), [Nenhum,Carro]),(Estrada (-1),[Carro,Nenhum]),(Estrada (-1),[Nenhum,Carro])])
mapaTVal3 = (Mapa 2 [(Relva, [Arvore,Nenhum]),(Relva, [Nenhum,Nenhum]),(Relva, [Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore])])
mapaTVal4 = (Mapa 8 [(Rio 1, [Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore]),(Rio (-2),[Nenhum,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum])])

mapaTVal1' = Mapa 5 [(Estrada (-1),[Nenhum,Carro,Carro,Carro,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco,Tronco]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco,Tronco])]
mapaTVal2' = Mapa 2 [(Rio (-1),[Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 1,[Nenhum,Carro]),(Estrada (-1),[Nenhum,Carro]),(Estrada (-1),[Carro,Nenhum]),(Estrada (-1),[Nenhum,Carro])]
mapaTVal3' = Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore])]
mapaTVal4' = Mapa 8 [(Rio (-1),[Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore]),(Rio (-2),[Nenhum,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum])]


-- Pares de (Terreno,[Obstaculo]) para testar a função proximosObstaculosValidos:

largura1 = 5
largura2 = 4

rio1 = (Rio 1, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])
estrada1 = (Estrada 2, [Carro,Carro,Nenhum,Carro,Nenhum,Carro])

rio2 = (Rio (-1), [])
estrada2 = (Estrada 1, [Nenhum,Carro,Carro,Nenhum])

relva = (Relva, [Arvore,Arvore,Arvore])
