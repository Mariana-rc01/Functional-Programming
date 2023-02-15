{- |
Module      : MultiPlayerTarefa3_2022li1g007
Description : Modo multiplayer
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module MultiPlayerTarefa3_2022li1g007 where

import LI12223
import Tarefa3_2022li1g007

{-| A função 'animaJogoM' movimenta os obstáculos (consoante a sua velocidade associada) e as personagens, de acordo com 
as jogadas fornecidas e onde estas se encontram, com o auxílio das funções 'moveObstaculoM' e 'moveJogador', respetivamente.

Assume-se que as personagens se encontram dentro de um mapa válido.

A função poderia ser definida da seguinte forma:

@
animaJogo :: JogoM -> [Jogada] -> JogoM
animaJogoM (JogoM jogador1 jogador2 mapa@(Mapa l lT)) [jogada1,jogada2] = JogoM (moveJogador jogador1 mapa jogada1) (moveJogador jogador2 mapa jogada2) (Mapa l (moveObstaculoM lT [jogada1,jogada2] jogador1 jogador2))
@

== Exemplos de utilização:

>>> animaJogoM (JogoM (Jogador (2,2)) (Jogador (2,1))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) [Parado,Parado]
JogoM (Jogador (2,2)) (Jogador (2,1)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogoM (JogoM (Jogador (2,2)) (Jogador (2,1))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) [Move Direita,Move Cima]
JogoM (Jogador (3,2)) (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogoM (JogoM (Jogador (1,0)) (Jogador (2,1))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) [Move Direita,Move Cima]
JogoM (Jogador (3,0)) (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])
-}

animaJogoM :: JogoM                    -- ^ 1º argumento: Jogo constituído por um jogador e por um mapa
                   -> [Jogada]         -- ^ 2º argumento: Jogadas efetuadas pelos jogadores respetivamente
                             -> JogoM  -- ^ resultado: Jogo com o movimento do jogador e dos obstáculos
animaJogoM (JogoM jogador1 jogador2 mapa@(Mapa l lT)) [jogada1,jogada2] = JogoM (moveJogador jogador1 mapa jogada1) (moveJogador jogador2 mapa jogada2) (Mapa l (moveObstaculoM lT [jogada1,jogada2] jogador1 jogador2))

{-| A função 'moveObstaculoM' tem como objetivo mover os obstáculos consoante a velocidade associada.

Desta forma, caso nos encontremos na __Relva__, os obstáculos mantêm-se na mesma posição, caso se encontrem na __Estrada__ ou no __Rio__,
os obstáculos movem-se.

É também necessário ter em atenção caso os Jogadores se encontrem ou se deslocem para uma Estrada, pois estes podem ser atropelados tendo em conta o movimento do mesmo.
De modo que, averigua-se quais os movimentos dos Jogadores e caso estes se movam para Cima ou para Baixo, verifica-se se o terreno para onde estes se movem é uma Estrada.
Caso contrário, verifica-se se os Jogadores se encontram numa Estrada.

Caso os Jogadores não se encontrem numa Estrada, então todos os obstáculos se irão mover sem qualquer restrição com o auxílio da função 'moveObstaculoAux'.

Caso um dos Jogadores se encontrem ou se deslocem para uma Estrada, esta tem os obstáculos a serem movidos com o auxílio da função 'moveEstrada' em função desse Jogador.
Os restantes terrenos têm os seus obstáculos a moverem-se sem qualquer restrição, movendo-se com o auxílio da função 'moveObstaculoAux'.

Caso ambos os Jogadores se encontrem ou se deslocem para uma Estrada, uma das Estradas será movida com o auxílio da função 'moveObstaculoMaux' (tendo em conta o movimento de um dos Jogadores) e os restantes terrenos com a função
'moveObstaculo' onde será movida também a outra Estrada (tendo em conta o movimento do outro Jogador).

A função poderia ser definida da seguinte forma:

@
moveObstaculoM :: [(Terreno,[Obstaculo])] -> [Jogada] -> Jogador -> Jogador -> [(Terreno,[Obstaculo])]
moveObstaculoM lT [jogada1,jogada2] jogador1 jogador2 | passaString verifica1 == "Estrada" && passaString verifica2 /= "Estrada" = moveObstaculo lT jogada1 jogador1
                                                      | passaString verifica1 /= "Estrada" && passaString verifica2 == "Estrada" = moveObstaculo lT jogada2 jogador2
                                                      | passaString verifica1 == "Estrada" && passaString verifica2 == "Estrada" = moveObstaculoMaux (moveObstaculo lT jogada1 jogador1) jogada2 jogador2
                                                      | otherwise = moveObstaculoAux lT
                                                      where verifica1 = fst $ verificaTerreno lT jogada1 jogador1
                                                            verifica2 = fst $ verificaTerreno lT jogada2 jogador2
                                                            passaString :: Terreno -> String
                                                            passaString t = case t of Estrada _ -> "Estrada"
                                                                                      Rio _     -> "Rio"    
                                                                                      Relva     -> "Relva"
@

== Exemplos de utilização:

>>> moveObstaculoM [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Nenhum])] [Move Direita,Parado] (Jogador (1,0)) (Jogador (2,2))
[(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])]

>>> moveObstaculoM [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum])] [Move Direita,Move Esquerda] (Jogador (1,0)) (Jogador (2,2))
[(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum])]

>>> moveObstaculoM [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])] [Parado,Move Esquerda] (Jogador (3,3)) (Jogador (2,2))
[(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Carro,Carro])]
-}

moveObstaculoM :: [(Terreno,[Obstaculo])]                                                            -- ^ 1º argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno
                                         -> [Jogada]                                                 -- ^ 2º argumento: Jogadas dos Jogadores
                                                    -> Jogador                                       -- ^ 3º argumento: 1º Jogador  
                                                              -> Jogador                             -- ^ 4º argumento: 2º Jogador  
                                                                         -> [(Terreno,[Obstaculo])]  -- ^ resultado: lista de pares de terrenos e lista de obstáculos movidos consoante a velocidade associada
moveObstaculoM lT [jogada1,jogada2] jogador1 jogador2 | passaString verifica1 == "Estrada" && passaString verifica2 /= "Estrada" = moveObstaculo lT jogada1 jogador1
                                                      | passaString verifica1 /= "Estrada" && passaString verifica2 == "Estrada" = moveObstaculo lT jogada2 jogador2
                                                      | passaString verifica1 == "Estrada" && passaString verifica2 == "Estrada" = moveObstaculoMaux (moveObstaculo lT jogada1 jogador1) jogada2 jogador2
                                                      | otherwise = moveObstaculoAux lT
                                                      where verifica1 = fst $ verificaTerreno lT jogada1 jogador1
                                                            verifica2 = fst $ verificaTerreno lT jogada2 jogador2
                                                            passaString :: Terreno -> String
                                                            passaString t = case t of Estrada _ -> "Estrada"
                                                                                      Rio _     -> "Rio"    
                                                                                      Relva     -> "Relva"

{-| A função 'verificaTerreno' tem como objetivo verificar em que Terreno o Jogador se encontra ou se desloca tendo em conta o seu movimento.

A função poderia ser definida da seguinte forma:

@
verificaTerreno :: [(Terreno,[Obstaculo])] -> Jogada -> Jogador -> (Terreno,[Obstaculo])
verificaTerreno lT m (Jogador (x,y)) | m == Move Cima     = lT !! (y-1)
                                     | m == Move Baixo    = lT !! (y+1)
                                     | otherwise          = lT !! y
@

== Exemplos de utilização:

>>> verificaTerreno [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])] (Move Esquerda) (Jogador (2,2))
(Estrada 3,[Carro,Carro,Nenhum,Nenhum])

>>> verificaTerreno [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])] (Move Cima) (Jogador (2,2))
(Relva,[Nenhum,Nenhum,Nenhum,Nenhum])
-}

verificaTerreno :: [(Terreno,[Obstaculo])]                                              -- ^ 1º argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno
                                          -> Jogada                                     -- ^ 2º argumento: Jogada do Jogador
                                                    -> Jogador                          -- ^ 3º argumento: Jogador
                                                               -> (Terreno,[Obstaculo]) -- ^ resultado: O par de terreno e lista de obstáculos em que o Jogador se encontra ou se vai deslocar
verificaTerreno lT m (Jogador (x,y)) | y > 0              && m == Move Cima     = lT !! (y-1)
                                     | y < length lT - 1  && m == Move Baixo    = lT !! (y+1)
                                     | otherwise          = lT !! y

{-| A função 'moveObstaculoMaux' tem como objetivo mover a Estrada em que um Jogador se encontra, não movendo os restantes terrenos.

Sabendo à priori que o Jogador se encontra ou se vai deslocar para uma Estrada.

Para ser possível mover em separado os obstáculos da Estrada que o Jogador se localiza, é usada a função splitAt, onde esta separa os terrenos, de modo que o último Terreno da
primeira parte do tuplo seja onde o Jogador se encontra, os restantes elementos da primeira parte do tuplo são os Terrenos que se encontram à frente do Jogador e a segunda parte do tuplo são
os Terrenos que se encontram atrás do Jogador.

A função poderia ser definida da seguinte forma:

@
moveObstaculoMaux :: [(Terreno,[Obstaculo])] -> Jogada -> Jogador -> [(Terreno,[Obstaculo])]
moveObstaculoMaux lT m (Jogador (x,y)) 
        | m == Move Direita  = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v (x+1))]  ++ (tras $ splitAt (y+1) lT)

        | m == Move Esquerda = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v (x-1))]  ++ (tras $ splitAt (y+1) lT)

        | m == Move Cima     = (frente $ splitAt y lT)     ++ [(Estrada vCima , moveEstrada (jogador $ splitAt y lT) vCima x)] ++ (tras $ splitAt y lT)

        | m == Move Baixo    = (frente $ splitAt (y+2) lT) ++ [(Estrada vBaixo, moveEstrada (jogador $ splitAt (y+2) lT) vBaixo x)] ++ (tras $ splitAt (y+2) lT)

        | m == Parado        = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v x)] ++ (tras $ splitAt (y+1) lT)
     where v = velocidade (fst(lT !! y))
           vCima = velocidade (fst (lT !! (y-1)))
           vBaixo = velocidade (fst (lT !! (y+1)))
           velocidade (Estrada x)  = x
           passaString :: Terreno -> String
           passaString t = case t of Estrada _ -> "Estrada"
                                     Rio _     -> "Rio"    
                                     Relva     -> "Relva" 
           frente x  = init $ fst x
           tras x    = snd x
           jogador x = snd $ last $ fst x
@

== Exemplos de utilização:

>>>  moveObstaculoMaux [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])] (Move Esquerda) (Jogador (2,2))
[(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])]

>>> moveObstaculoMaux [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum])] (Move Baixo) (Jogador (3,2))
[(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Carro,Carro])]
-}

moveObstaculoMaux :: [(Terreno,[Obstaculo])]                                            -- ^ 1º argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno
                                        -> Jogada                                       -- ^ 2º argumento: Jogada do Jogador
                                                  -> Jogador                            -- ^ 3º argumento: Jogador
                                                             -> [(Terreno,[Obstaculo])] -- ^ resultado: lista de pares de terrenos e lista de obstáculos movidos consoante a velocidade associada
moveObstaculoMaux lT m (Jogador (x,y)) 
        | m == Move Direita  = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v (x+1))]  ++ (tras $ splitAt (y+1) lT)

        | m == Move Esquerda = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v (x-1))]  ++ (tras $ splitAt (y+1) lT)

        | m == Move Cima     = (frente $ splitAt y lT)     ++ [(Estrada vCima , moveEstrada (jogador $ splitAt y lT) vCima x)] ++ (tras $ splitAt y lT)

        | m == Move Baixo    = (frente $ splitAt (y+2) lT) ++ [(Estrada vBaixo, moveEstrada (jogador $ splitAt (y+2) lT) vBaixo x)] ++ (tras $ splitAt (y+2) lT)

        | m == Parado        = (frente $ splitAt (y+1) lT) ++ [(Estrada v, moveEstrada (jogador $ splitAt (y+1) lT) v x)] ++ (tras $ splitAt (y+1) lT)
     where v = velocidade (fst(lT !! y))
           vCima = velocidade (fst (lT !! (y-1)))
           vBaixo = velocidade (fst (lT !! (y+1)))
           velocidade (Estrada x)  = x
           passaString :: Terreno -> String
           passaString t = case t of Estrada _ -> "Estrada"
                                     Rio _     -> "Rio"    
                                     Relva     -> "Relva" 
           frente x  = init $ fst x
           tras x    = snd x
           jogador x = snd $ last $ fst x

{-| A função 'animaJogadorTroncoM1' move a primeira personagem, de acordo com 
a jogada fornecida e onde a personagem se encontra, com o auxílio da função 'moveJogador' e, 
caso o jogador se encontre num rio, move apenas os Troncos desse rio, com o auxílio da função 'moveTronco'.

Assume-se que a personagem se encontra dentro de um mapa válido.

A função poderia ser definida da seguinte forma:

@
animaJogadorTroncoM1 :: JogoM -> Jogada -> JogoM
animaJogadorTroncoM1 (JogoM jogador1 jogador2 mapa@(Mapa l lT)) jogada1 = JogoM (moveJogador jogador1 mapa jogada1) jogador2 (Mapa l (moveTronco lT jogador1))
@

== Exemplos de utilização:

>>> animaJogadorTroncoM1 (JogoM (Jogador (2,2)) (Jogador (2,1))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) (Move Cima)
JogoM (Jogador (2,1)) (Jogador (2,1)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> animaJogadorTroncoM1 (JogoM (Jogador (1,0)) (Jogador (2,1))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
JogoM (Jogador (2,0)) (Jogador (2,1)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])
-}

animaJogadorTroncoM1 :: JogoM                   -- ^ 1º argumento: Jogo constituído por um jogador e por um mapa
                              -> Jogada         -- ^ 2º argumento: Jogada efetuada
                                       -> JogoM -- ^ resultado: Jogo com o movimento do jogador e dos obstáculos
animaJogadorTroncoM1 (JogoM jogador1 jogador2 mapa@(Mapa l lT)) jogada1 = JogoM (moveJogador jogador1 mapa jogada1) jogador2 (Mapa l (moveTronco lT jogador1))

{-| A função 'animaJogadorTroncoM2' move a segunda personagem, de acordo com 
a jogada fornecida e onde a personagem se encontra, com o auxílio da função 'moveJogador' e, 
caso o jogador se encontre num rio, move apenas os Troncos desse rio, com o auxílio da função 'moveTronco'.

Assume-se que a personagem se encontra dentro de um mapa válido.

A função poderia ser definida da seguinte forma:

@
animaJogadorTroncoM2 :: JogoM -> Jogada -> JogoM
animaJogadorTroncoM2 (JogoM jogador1 jogador2 mapa@(Mapa l lT)) jogada2 = JogoM jogador1 (moveJogador jogador2 mapa jogada2) (Mapa l (moveTronco lT jogador2))
@

== Exemplos de utilização:

>>> animaJogadorTroncoM2 (JogoM (Jogador (1,1)) (Jogador (1,0))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
JogoM (Jogador (1,1)) (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> animaJogadorTroncoM2 (JogoM (Jogador (1,1)) (Jogador (1,0))  (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) (Move Baixo)
JogoM (Jogador (1,1)) (Jogador (1,1)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])
-}

animaJogadorTroncoM2 :: JogoM                   -- ^ 1º argumento: Jogo constituído por um jogador e por um mapa
                              -> Jogada         -- ^ 2º argumento: Jogada efetuada
                                       -> JogoM -- ^ resultado: Jogo com o movimento do jogador e dos obstáculos
animaJogadorTroncoM2 (JogoM jogador1 jogador2 mapa@(Mapa l lT)) jogada2 = JogoM jogador1 (moveJogador jogador2 mapa jogada2) (Mapa l (moveTronco lT jogador2))
