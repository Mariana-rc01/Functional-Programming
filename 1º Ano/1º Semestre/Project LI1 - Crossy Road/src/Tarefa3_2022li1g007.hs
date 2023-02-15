{- |
Module      : Tarefa3_2022li1g007
Description : Movimentação do personagem e obstáculos
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}

module Tarefa3_2022li1g007 where

import LI12223

{-| A função 'animaJogo' movimenta os obstáculos (consoante a sua velocidade associada) e a personagem, de acordo com 
a jogada fornecida e onde a personagem se encontra, com o auxílio das funções 'moveObstaculo' e 'moveJogador', respetivamente.

Assume-se que a personagem se encontra dentro de um mapa válido.

A função poderia ser definida da seguinte forma:

@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo jogo@(Jogo jogador (Mapa l lT)) jogada = Jogo (moveJogador jogo jogada) (Mapa l (moveObstaculo lT))
@

== Exemplos de utilização:

>>> animaJogo (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogo (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogo (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) (Move Baixo)
Jogo (Jogador (1,1)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogo (Jogo (Jogador (3,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Carro]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])])) (Move Cima)
Jogo (Jogador (3,1)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Carro,Carro,Carro]),(Rio 2,[Tronco,Nenhum,Tronco,Tronco])])

-}

animaJogo :: Jogo                   -- ^ 1º argumento: Jogo constituído por um jogador e por um mapa
                  -> Jogada         -- ^ 2º argumento: Jogada efetuada
                            -> Jogo -- ^ resultado: Jogo com o movimento do jogador e dos obstáculos
animaJogo (Jogo jogador mapa@(Mapa l lT)) jogada = Jogo (moveJogador jogador mapa jogada) (Mapa l (moveObstaculo lT jogada jogador))

{-| A função 'moveJogador' recebe um Jogador, o Mapa em que este se encontra e uma Jogada e retorna um Jogador com a sua posição após a jogada.

Desta forma, caso a jogada seja __Parado__ a sua posição final será a mesma, caso o Jogador não se encontre em cima de 
um Tronco, caso contrário, este move-se consoante a velocidade do Rio.

Caso esteja definido um __movimento__ na Jogada, a posição final do Jogador terá em conta quando o Jogador:

- Se encontra em cima de um Tronco

- Tenta escapar do mapa através dos seus movimentos

- Tenta ir para a mesma posição que uma Arvore

Note-se que, caso o Jogador se encontre em cima de um Tronco e pretenda ir para avançar e se deparar com uma Arvore, então
o Jogador move-se com o Tronco e não consegue avançar para a Relva.
Caso o Jogador esteja em cima de um Tronco e pretenda avançar para outro Terreno, a sua coordenada horizontal não é 
afetada pela velocidade do Rio em que se encontra.

A função poderia ser definida da seguinte forma:

@
moveJogador :: Jogo -> Mapa -> Jogada -> Jogador

moveJogador (Jogador (x,y)) (Mapa l lT) Parado | ((snd(lT !! y)) !! x) == Tronco = Jogador (x+velocidade,y)
                                               | otherwise                       = Jogador (x,y)
                                               where velocidade = v(fst(lT !! y))
                                                     v (Rio x)  = x

moveJogador (Jogador (x,y)) (Mapa l lT) move | obstaculo == Tronco && move == Move Esquerda                                                   = Jogador (x+velocidade-1,y)
                                             | obstaculo == Tronco && move == Move Direita                                                    = Jogador (x+velocidade+1,y)
                                             | obstaculo == Tronco && (move == Move Cima  && (y == 0 || ((snd(lT !! (y-1))) !! x) == Arvore)) = Jogador (x+velocidade,y)
                                             | obstaculo == Tronco && (move == Move Baixo && ((snd(lT !! (y+1))) !! x) == Arvore)             = Jogador (x+velocidade,y)
                                             where velocidade = v(fst(lT !! y))
                                                   v (Rio x)  = x
                                                   obstaculo = ((snd(lT !! y)) !! x)

moveJogador j@(Jogador (x,y)) (Mapa l lT) move | move == Move Cima     && y /= 0            && ((snd(lT !! (y-1))) !! x) == Arvore = j
                                               | move == Move Baixo    && y < length lT - 1 && ((snd(lT !! (y+1))) !! x) == Arvore = j
                                               | move == Move Esquerda && x /= 0            && ((snd(lT !! y)) !! (x-1)) == Arvore = j
                                               | move == Move Direita  && x /= l-1          && ((snd(lT !! y)) !! (x+1)) == Arvore = j  

moveJogador j@(Jogador (x,y)) (Mapa l lT) move | y == 0             && move == Move Cima     = j
                                               | x == 0             && move == Move Esquerda = j
                                               | x == l-1           && move == Move Direita  = j
                                               | y == length lT - 1 && move == Move Baixo    = j

moveJogador (Jogador (x,y)) _ move = case move of Move Cima     -> Jogador (x,y-1)
                                                  Move Baixo    -> Jogador (x,y+1)
                                                  Move Esquerda -> Jogador (x-1,y)
                                                  Move Direita  -> Jogador (x+1,y)
@

== Exemplos de utilização:

>>> moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum]),(Rio 2, [Nenhum, Tronco])])) Parado
Jogador (3,0)

>>> moveJogador (Jogo (Jogador (2,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])])) (Move Cima)
Jogador (4,2)

>>> moveJogador (Jogo (Jogador (2,2)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum])])) (Move Esquerda)
Jogador (3,2)
-}

moveJogador :: Jogador                               -- ^ 1º argumento: Jogador que se vai mover
                      -> Mapa                        -- ^ 2º argumento: Mapa em que o Jogador se encontra
                              -> Jogada              -- ^ 3º argumento: Jogada efetuada
                                        -> Jogador   -- ^ resultado: Jogo com o movimento do jogador

moveJogador (Jogador (x,y)) (Mapa l lT) Parado | ((snd(lT !! y)) !! x) == Tronco = Jogador (x+velocidade,y)
                                               | otherwise                       = Jogador (x,y)
                                               where velocidade = v(fst(lT !! y))
                                                     v (Rio x)  = x

moveJogador (Jogador (x,y)) (Mapa l lT) move | obstaculo == Tronco && move == Move Esquerda                                                   = Jogador (x+velocidade-1,y)
                                             | obstaculo == Tronco && move == Move Direita                                                    = Jogador (x+velocidade+1,y)
                                             | obstaculo == Tronco && (move == Move Cima  && (y == 0 || ((snd(lT !! (y-1))) !! x) == Arvore)) = Jogador (x+velocidade,y)
                                             | obstaculo == Tronco && (move == Move Baixo && ((snd(lT !! (y+1))) !! x) == Arvore)             = Jogador (x+velocidade,y)
                                             where velocidade = v(fst(lT !! y))
                                                   v (Rio x)  = x
                                                   obstaculo = ((snd(lT !! y)) !! x)

moveJogador j@(Jogador (x,y)) (Mapa l lT) move | move == Move Cima     && y /= 0            && ((snd(lT !! (y-1))) !! x) == Arvore = j
                                               | move == Move Baixo    && y < length lT - 1 && ((snd(lT !! (y+1))) !! x) == Arvore = j
                                               | move == Move Esquerda && x /= 0            && ((snd(lT !! y)) !! (x-1)) == Arvore = j
                                               | move == Move Direita  && x /= l-1          && ((snd(lT !! y)) !! (x+1)) == Arvore = j  

moveJogador j@(Jogador (x,y)) (Mapa l lT) move | y == 0             && move == Move Cima     = j
                                               | x == 0             && move == Move Esquerda = j
                                               | x == l-1           && move == Move Direita  = j
                                               | y == length lT - 1 && move == Move Baixo    = j

moveJogador (Jogador (x,y)) _ move = case move of Move Cima     -> Jogador (x,y-1)
                                                  Move Baixo    -> Jogador (x,y+1)
                                                  Move Esquerda -> Jogador (x-1,y)
                                                  Move Direita  -> Jogador (x+1,y)

{-| A função 'moveObstaculo' tem como objetivo mover os obstáculos consoante a velocidade associada.

Desta forma, caso nos encontremos na __Relva__, os obstáculos mantêm-se na mesma posição, caso se encontrem na __Estrada__ ou no __Rio__,
os obstáculos movem-se.

É também necessário ter em atenção caso o Jogador se encontre ou se desloce para uma Estrada que este pode ser atropelado tendo em conta o movimento do mesmo.
De modo que, averigua-se qual o movimento do Jogador e caso este se mova para Cima ou para Baixo, verifica-se se o terreno para onde este se move é uma Estrada.
Caso contrário, verifica-se se o Jogador se encontra numa Estrada.

Caso o Jogador não se encontre numa Estrada, então todos os obstáculos se irão mover sem qualquer restrição com o auxílio da função 'moveObstaculoAux'.

Caso o Jogador se encontre ou se desloce para uma Estrada, esta tem os obstáculos a serem movidos com o auxílio da função auxiliar 'moveEstrada'.
Os restantes terrenos têm os seus obstáculos a moverem-se sem qualquer restrição, movendo-se com o auxílio da função 'moveObstaculoAux'.

Para ser possível mover em separado os obstáculos da Estrada que o Jogador se localiza, é usada a função splitAt, onde esta separa os terrenos, de modo que o último Terreno da
primeira parte do tuplo seja onde o Jogador se encontra, os restantes elementos da primeira parte do tuplo são os Terrenos que se encontram à frente do Jogador e a segunda parte do tuplo são
os Terrenos que se encontram atrás do Jogador.

A função poderia ser definida da seguinte forma:

@
moveObstaculo :: [(Terreno,[Obstaculo])] -> Jogada -> Jogador -> [(Terreno,[Obstaculo])]
moveObstaculo lT@((t,obs):ts) m (Jogador (x,y)) 
        | m == Move Direita  && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v (x+1))]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | m == Move Esquerda && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v (x-1))]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | m == Parado        && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v x)]      ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | y > 0 && m == Move Cima && (passaString (fst(lT !! (y-1)))) == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v x)]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | y == 0 && m == Move Cima && (passaString (fst(lT !! y))) == "Estrada"    = moveObstaculoAux (frente $ splitAt y lT)     ++ [(Estrada vCima , moveEstrada (jogador $ splitAt y lT) vCima x)]  ++ moveObstaculoAux (tras $ splitAt y lT)

        | y < length lT - 1  && m == Move Baixo && (passaString (fst(lT !! (y+1)))) == "Estrada" = moveObstaculoAux (frente $ splitAt (y+2) lT) ++ [(Estrada vBaixo, moveEstrada (jogador $ splitAt (y+2) lT) vBaixo x)] ++ moveObstaculoAux (tras $ splitAt (y+2) lT)

        | y == length lT - 1 && m == Move Baixo && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v x)]      ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | otherwise = moveObstaculoAux lT

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

>>> moveObstaculo [(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum])] (Move Baixo) (Jogador (1,1))
[(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Carro])]

>>> moveObstaculo [(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum])] (Move Cima) (Jogador (1,1))
[(Estrada 1,[Nenhum,Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum])]

>>> moveObstaculo [(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum])] (Move Esquerda) (Jogador (1,0))
[(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum])]
-}

moveObstaculo :: [(Terreno,[Obstaculo])]                                                -- ^ 1º argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno
                                        -> Jogada                                       -- ^ 2º argumento: Jogada do Jogador
                                                  -> Jogador                            -- ^ 3º argumento: Jogador        
                                                             -> [(Terreno,[Obstaculo])] -- ^ resultado: lista de pares de terrenos e lista de obstáculos movidos consoante a velocidade associada
moveObstaculo lT@((t,obs):ts) m (Jogador (x,y)) 
        | m == Move Direita  && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v (x+1))]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | m == Move Esquerda && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v (x-1))]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | m == Parado        && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v x)]      ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | y > 0 && m == Move Cima && (passaString (fst(lT !! (y-1)))) == "Estrada" = moveObstaculoAux (frente $ splitAt y lT) ++ [(Estrada vCima, moveEstrada (jogador $ splitAt y lT) vCima x)]  ++ moveObstaculoAux (tras $ splitAt y lT)

        | y == 0 && m == Move Cima && (passaString (fst(lT !! y))) == "Estrada"    = moveObstaculoAux (frente $ splitAt (y+1) lT)     ++ [(Estrada v    , moveEstrada (jogador $ splitAt (y+1) lT) vCima x)]  ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | y < length lT - 1  && m == Move Baixo && (passaString (fst(lT !! (y+1)))) == "Estrada" = moveObstaculoAux (frente $ splitAt (y+2) lT) ++ [(Estrada vBaixo, moveEstrada (jogador $ splitAt (y+2) lT) vBaixo x)] ++ moveObstaculoAux (tras $ splitAt (y+2) lT)

        | y == length lT - 1 && m == Move Baixo && (passaString (fst(lT !! y)))     == "Estrada" = moveObstaculoAux (frente $ splitAt (y+1) lT) ++ [(Estrada v     , moveEstrada (jogador $ splitAt (y+1) lT) v x)]      ++ moveObstaculoAux (tras $ splitAt (y+1) lT)

        | otherwise = moveObstaculoAux lT

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

{-| A função 'moveEstrada' tem como objetivo mover os obstáculos consoante a velocidade associada tendo em conta onde o Jogador se encontra.

Caso um Carro esteja na posição onde o Jogador se encontra, então os obstáculos param de se mexer, pois o jogador é atropelado durante o movimento do carro,
correspondendo ao estado do mapa no momento em que o jogador perdeu.

Caso contrário, os obstáculos movem-se consoante a sua velocidade.


A função poderia ser definida da seguinte forma:

@
moveEstrada :: [Obstaculo] -> Int -> Int -> [Obstaculo]
moveEstrada l 0 _ = l
moveEstrada l v x | v > 0 && (l !! x) /= Carro = moveEstrada (last l: init l) (v-1) x
                  | v < 0 && (l !! x) /= Carro = moveEstrada (tail l ++ [head l]) (v+1) x
                  | otherwise = l
@

== Exemplos de utilização:

>>> moveEstrada [Carro,Carro,Nenhum,Nenhum] 2 2
[Nenhum,Carro,Carro,Nenhum]

>>> moveEstrada [Carro,Carro,Nenhum,Nenhum,Nenhum] 2 4
[Nenhum,Nenhum,Carro,Carro,Nenhum]
-}          

moveEstrada :: [Obstaculo] -- ^ 1º argumento: lista de obstáculos
                           -> Int -- ^ 2º argumento: velocidade dos obstáculos
                                  -> Int -- ^ 3º argumento: posição do jogador no terreno
                                         -> [Obstaculo] -- ^ resultado: lista de obstáculos movidos consoante a velocidade associada
moveEstrada l 0 _ = l
moveEstrada l v x | v > 0 && (l !! x) /= Carro = moveEstrada (last l: init l) (v-1) x
                  | v < 0 && (l !! x) /= Carro = moveEstrada (tail l ++ [head l]) (v+1) x
                  | otherwise = l

{-| A função 'moveObstaculoAux' tem como objetivo mover os obstáculos consoante a velocidade associada.

Desta forma, caso nos encontremos na __Relva__, os obstáculos mantêm-se na mesma posição, caso se encontrem na __Estrada__ ou no __Rio__,
os obstáculos movem-se.

Para que seja possível mover os obstáculos, é necessário ter em conta que os obstáculos devem se mover |v| unidades na direção 
determinada. Assim, caso a velocidade seja __positiva__, os obstáculos devem-se mover __da__ __esquerda__ __para__ __a__ __direita__ |v| unidades,
caso seja __negativa__, devem-se mover __da__ __direita__ __para__ __a__ __esquerda__ |v| unidades.


A função poderia ser definida da seguinte forma:

@
moveObstaculoAux :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObstaculoAux [] = []
moveObstaculoAux ((t,l):ts) = case t of Relva     -> (Relva,l):moveObstaculoAux ts
                                        Rio v     -> (Rio v, (auxMove l v)):moveObstaculoAux ts
                                        Estrada v -> (Estrada v, (auxMove l v)):moveObstaculoAux ts
                    where auxMove :: [Obstaculo] -> Int -> [Obstaculo]
                          auxMove l 0 = l
                          auxMove l v | v > 0 = auxMove (last l: init l) (v-1)
                                      | v < 0 = auxMove (tail l ++ [head l]) (v+1)
@

== Exemplos de utilização:

>>> moveObstaculoAux [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum])]
[(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro])]

>>> moveObstaculoAux [(Rio 3,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-4),[Carro,Carro,Nenhum,Nenhum])]
[(Rio 3,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-4),[Carro,Carro,Nenhum,Nenhum])]

-}

moveObstaculoAux :: [(Terreno,[Obstaculo])]                          -- ^ argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno                                      
                                           -> [(Terreno,[Obstaculo])]-- ^ resultado: lista de pares de terrenos e lista de obstáculos movidos consoante a velocidade associada
moveObstaculoAux [] = []
moveObstaculoAux ((t,l):ts) = case t of Relva     -> (Relva,l):moveObstaculoAux ts
                                        Rio v     -> (Rio v, (auxMove l v)):moveObstaculoAux ts
                                        Estrada v -> (Estrada v, (auxMove l v)):moveObstaculoAux ts
                    where auxMove :: [Obstaculo] -> Int -> [Obstaculo]
                          auxMove l 0 = l
                          auxMove l v | v > 0 = auxMove (last l: init l) (v-1)
                                      | v < 0 = auxMove (tail l ++ [head l]) (v+1)

{-| A função 'animaJogadorTronco' move a personagem, de acordo com 
a jogada fornecida e onde a personagem se encontra, com o auxílio da função 'moveJogador' e, 
caso o jogador se encontre num rio, move apenas os Troncos desse rio.

Assume-se que a personagem se encontra dentro de um mapa válido.

A função poderia ser definida da seguinte forma:

@
animaJogadorTronco :: Jogo -> Jogada -> Jogo
animaJogadorTronco (Jogo jogador mapa@(Mapa l lT)) jogada = Jogo (moveJogador jogador mapa jogada) (Mapa l (moveTronco lT jogador))
@

== Exemplos de utilização:

>>> animaJogadorTronco (Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
Jogo (Jogador (2,2)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

>>> animaJogo (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])])

>>> animaJogadorTronco (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])) Parado
Jogo (Jogador (2,0)) (Mapa 4 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])])

-}

animaJogadorTronco :: Jogo                   -- ^ 1º argumento: Jogo constituído por um jogador e por um mapa
                           -> Jogada         -- ^ 2º argumento: Jogada efetuada
                                     -> Jogo -- ^ resultado: Jogo com o movimento do jogador e dos obstáculos
animaJogadorTronco (Jogo jogador mapa@(Mapa l lT)) jogada = Jogo (moveJogador jogador mapa jogada) (Mapa l (moveTronco lT jogador))

{-| A função 'moveTronco' tem como objetivo mover os Troncos de um certo Rio, caso o Jogador se encontre nesse Rio.

Desta forma, caso nos encontremos na __Relva__ ou na __Estrada__, os obstáculos mantêm-se na mesma posição, caso se encontrem no __Rio__,
os obstáculos movem-se com o auxílio da função 'moveObstaculoAux'.

A função poderia ser definida da seguinte forma:

@
moveTronco :: [(Terreno,[Obstaculo])] -> Jogador -> [(Terreno,[Obstaculo])]
moveTronco lT (Jogador (x,y)) 
        | (passaString (fst(lT !! y))) == "Rio" = (frente $ splitAt (y+1) lT) ++ moveObstaculoAux [(Rio v, jogador $ splitAt (y+1) lT)]  ++ (tras $ splitAt (y+1) lT)
        | (passaString (fst(lT !! y))) == "Rio" = (frente $ splitAt (y+1) lT) ++ moveObstaculoAux [(Rio v, jogador $ splitAt (y+1) lT)]  ++ (tras $ splitAt (y+1) lT)
        | otherwise = lT
     where v = velocidade (fst(lT !! y))
           velocidade (Rio x)  = x
           passaString :: Terreno -> String
           passaString t = case t of Estrada _ -> "Estrada"
                                     Rio _     -> "Rio"    
                                     Relva     -> "Relva" 
           frente x  = init $ fst x
           tras x    = snd x
           jogador x = snd $ last $ fst x
@

== Exemplos de utilização:

>>> moveTronco [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])] (Jogador (1,0))
[(Rio 1,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]

>>> moveTronco [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])] (Jogador (1,1))
[(Rio 1,[Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum])]

-}

moveTronco :: [(Terreno,[Obstaculo])]                                       -- ^ 1º argumento: lista de pares de terrenos e lista de obstáculos associada a cada terreno
                                      -> Jogador                            -- ^ 2º argumento: Jogador        
                                                 -> [(Terreno,[Obstaculo])] -- ^ resultado: lista de pares de terrenos e lista de obstáculos movidos consoante a velocidade associada
moveTronco lT (Jogador (x,y)) 
        | (passaString (fst(lT !! y))) == "Rio" = (frente $ splitAt (y+1) lT) ++ moveObstaculoAux [(Rio v, jogador $ splitAt (y+1) lT)]  ++ (tras $ splitAt (y+1) lT)
        | (passaString (fst(lT !! y))) == "Rio" = (frente $ splitAt (y+1) lT) ++ moveObstaculoAux [(Rio v, jogador $ splitAt (y+1) lT)]  ++ (tras $ splitAt (y+1) lT)
        | otherwise = lT
     where v = velocidade (fst(lT !! y))
           velocidade (Rio x)  = x
           passaString :: Terreno -> String
           passaString t = case t of Estrada _ -> "Estrada"
                                     Rio _     -> "Rio"    
                                     Relva     -> "Relva" 
           frente x  = init $ fst x
           tras x    = snd x
           jogador x = snd $ last $ fst x