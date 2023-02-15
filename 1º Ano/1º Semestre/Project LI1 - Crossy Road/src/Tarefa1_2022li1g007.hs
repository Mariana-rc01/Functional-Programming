{- |
Module      : Tarefa1_2022li1g007
Description : Validação de um mapa
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}

module Tarefa1_2022li1g007 where

import LI12223
import Data.List

{-| A função 'mapaValido' recebe um mapa e verifica se este cumpre todas as restrições realizadas 
nas funções de restrição que constituem a função 'mapaValido'.

A função poderia ser definida da seguinte forma:

@
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = False
mapaValido mapa = validaObst mapa && validaRios mapa && validaComp mapa && validaCaminho mapa && validaTamanho mapa && validaSeq mapa 
@ 

== Exemplos de utilização:

>>> mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
True

>>> mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Carro, Carro, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
False
-}

mapaValido :: Mapa         -- ^ argumento: um mapa
                   -> Bool -- ^ resultado: True caso o mapa cumpra todos os requesitos impostos pelas funções, caso contrário False 
mapaValido (Mapa _ []) = False
mapaValido mapa = validaObst mapa && validaRios mapa && validaComp mapa && validaCaminho mapa && validaTamanho mapa && validaSeq mapa && validaArvores mapa

{-| A função 'validaObst' recebe um mapa e verifica se este não contém objetos em terrenos impróprios.
Assim, um mapa que contenha um __Tronco__ ou uma __Arvore__ no terreno __Estrada__, por exemplo, é um mapa inválido e assim a função devolve __False__.
O mesmo acontece para o terreno __Rio__ caso contenha __Arvore__ ou __Carro__ e para o terreno __Relva__ caso tenha __Tronco__ ou __Carro__.

A função poderia ser definida da seguinte forma:

@
validaObst :: Mapa -> Bool 
validaObst (Mapa _ []) = True
validaObst (Mapa l ((Relva,lT):t))      = if elem Tronco lT || elem Carro lT  then False else validaObst (Mapa l t)
validaObst (Mapa l ((Estrada _,lT):t))  = if elem Tronco lT || elem Arvore lT then False else validaObst (Mapa l t)
validaObst (Mapa l ((Rio _,lT):t))      = if elem Arvore lT || elem Carro lT  then False else validaObst (Mapa l t)
@

== Exemplos de utilização:

>>> validaObst (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
True
>>> validaObst (Mapa 5 [(Relva, [Arvore, Nenhum, Carro, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
False
-}

validaObst :: Mapa         -- ^ argumento: um mapa
                   -> Bool -- ^ resultado: True caso o mapa não contenha objetos em terrenos impróprios, caso contrário False
validaObst (Mapa _ []) = True
validaObst (Mapa l ((Relva,lT):t))      = if elem Tronco lT || elem Carro lT  then False else validaObst (Mapa l t)
validaObst (Mapa l ((Estrada _,lT):t))  = if elem Tronco lT || elem Arvore lT then False else validaObst (Mapa l t)
validaObst (Mapa l ((Rio _,lT):t))      = if elem Arvore lT || elem Carro lT  then False else validaObst (Mapa l t)

{-| A função 'validaRios' recebe um mapa e verifica se rios contíguos têm direções opostas.
Para isso, ela assume as velocidades de __dois Rios__ que ocorrem __consecutivamente__ e averigua se estes tem velocidades com __sinais__ __opostos__.
Caso tal não aconteça, o mapa é inválido e a função retorna False.

A função poderia ser definida da seguinte forma:

@
validaRios :: Mapa -> Bool
validaRios (Mapa _ []) = True
validaRios (Mapa _ [(Rio _,_)]) = True
validaRios (Mapa l ((Rio v1,lT1):(Rio v2,lT2):t))
   |v1 * v2 < 0 = validaRios (Mapa l ((Rio v2,lT2):t))
   |otherwise = False
validaRios (Mapa l ((Rio v, lT):h1:t)) = case h1 of (Estrada _,_) -> validaRios (Mapa l t)
                                                    (Relva, _)    -> validaRios (Mapa l t)
validaRios (Mapa l ((e,_):t)) = case e of Estrada _ -> validaRios (Mapa l t)
                                          Relva     -> validaRios (Mapa l t)
@

== Exemplos de utilização:

>>> validaRios (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
True

>>> validaRios (Mapa 2 [(Rio 1,[Tronco, Nenhum]),(Rio (-1),[Nenhum, Tronco]),(Rio 1,[Tronco, Nenhum])])
True

>>> validaRios (Mapa 2 [(Rio 1,[Tronco, Nenhum]),(Rio 2,[Nenhum, Tronco]),(Rio 1,[Tronco, Nenhum])])
False
-}

validaRios :: Mapa          -- ^ argumento: um mapa
                   -> Bool  -- ^ resultado: True caso as velocidades de dois rios tenham sinais opostos, caso contrário, False
validaRios (Mapa _ []) = True
validaRios (Mapa _ [(Rio _,_)]) = True
validaRios (Mapa l ((Rio v1,lT1):(Rio v2,lT2):t))
   |v1 * v2 < 0 = validaRios (Mapa l ((Rio v2,lT2):t))
   |otherwise = False
validaRios (Mapa l ((Rio v, lT):h1:t)) = case h1 of (Estrada _,_) -> validaRios (Mapa l t)
                                                    (Relva, _)    -> validaRios (Mapa l t)
validaRios (Mapa l ((e,_):t)) = case e of Estrada _ -> validaRios (Mapa l t)
                                          Relva     -> validaRios (Mapa l t)

{-| A função 'validaComp' recebe um mapa e verifica se os Troncos têm no __máximo__ __5__ __unidades__ de comprimento e os __Carros__ no __máximo__ __3__ unidades.
Assim, ela verifica se estes elementos não ocorrem consecutivamente na lista mais vezes do que aquelas que lhes são permitidas, com o auxílio das funções 'validaComp1' e 'validaComp2'.
No caso do Carro, caso ocorra [Carro, Carro, Carro, Carro, Nenhum] a função devolverá False já que este ocorre mais do que três vezes consecutivas na lista ultrapassando o tamanho permitido.

A função poderia ser definida da seguinte forma:

@
validaComp :: Mapa -> Bool
validaComp (Mapa _ [])             = True
validaComp (Mapa l ((_,[]):t))     = validaComp (Mapa l t)
validaComp (Mapa l ((Relva,_):t))  = validaComp (Mapa l t)
validaComp mapa@(Mapa l ((e,lT):t)) = if head lT /= last lT then validaComp1 mapa else validaComp2 mapa
@

== Exemplos de utilização:
>>> validaComp (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Nenhum, Tronco]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
True

>>> validaComp (Mapa 6 [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Nenhum])])
False

>>> validaComp (Mapa 6 [(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco]),(Estrada 1, [Carro, Carro, Carro, Carro, Nenhum, Nenhum])])
False
-}

validaComp :: Mapa         -- ^ argumento: um mapa
                   -> Bool -- ^ resultado: True caso não haja carros com comprimento superior a 3 e troncos com comprimento superior a 5, caso contrário False
validaComp (Mapa _ [])             = True
validaComp (Mapa l ((_,[]):t))     = validaComp (Mapa l t)
validaComp (Mapa l ((Relva,_):t))  = validaComp (Mapa l t)
validaComp mapa@(Mapa l ((e,lT):t)) = if head lT /= last lT then validaComp1 mapa else validaComp2 mapa

{-| A função 'validaComp1' recebe um mapa e verifica se na lista de obstáculos é respeitada a restrição
da função 'validaComp', quando o primeiro elemento é diferente do último, ou seja, não são verificados os casos 
em que o __Carro__ ou o __Tronco__ estão "divididos" (por exemplo [Carro,Nenhum,Carro]).

A função poderia ser definida da seguinte forma:

@
validaComp1 :: Mapa -> Bool
validaComp1 (Mapa _ []) = True
validaComp1 (Mapa l ((_,[]):t)) = validaComp (Mapa l t)
validaComp1 (Mapa l ((e,lT):t)) |all (== Carro)  (take 4 lT) && length lT > 3 = False
                                |all (== Tronco) (take 6 lT) && length lT > 5 = False 
                                |otherwise = validaComp1 (Mapa l ((e, tail lT):t))
@

== Exemplos de utilização:
>>> validaComp1 (Mapa 4 [(Relva, [Nenhum, Arvore, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Tronco]),(Rio (-2), [Tronco, Nenhum, Tronco, Nenhum])])
True

>>> validaComp1 (Mapa 5 [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco]),(Rio (-2), [Tronco, Nenhum, Nenhum, Tronco, Nenhum])])
False

>>> validaComp1 (Mapa 6 [(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco]),(Estrada 1, [Carro, Carro, Carro, Carro, Nenhum, Nenhum])])
False
-}

validaComp1 :: Mapa -> Bool
validaComp1 (Mapa _ []) = True
validaComp1 (Mapa l ((_,[]):t)) = validaComp (Mapa l t)
validaComp1 (Mapa l ((e,lT):t)) |all (== Carro)  (take 4 lT) && length lT > 3 = False
                                |all (== Tronco) (take 6 lT) && length lT > 5 = False 
                                |otherwise = validaComp1 (Mapa l ((e, tail lT):t))

{-| A função 'validaComp2' recebe um mapa e verifica se na lista de obstáculos é respeitada a restrição
da função 'validaComp', nos casos em que o __Carro__ e o __Tronco__ estão "divididos", por exemplo, [Carro,Nenhum,Carro] ou 
[Tronco,Tronco,Nenhum,Tronco].

A função poderia ser definida da seguinte forma:

@
validaComp2 :: Mapa -> Bool
validaComp2 (Mapa _ []) = True
validaComp2 (Mapa l ((_,[]):t)) = validaComp (Mapa l t)
validaComp2 mapa@(Mapa x ((e,l):t)) | head l == Carro  = if all (== Carro) l  || nElementos > 3 then False else validaComp1 mapa
                                    | head l == Tronco = if all (== Tronco) l || nElementos > 5 then False else validaComp1 mapa
                                    | otherwise        = validaComp1 mapa
                                    where nElementos = length hl + length (last tl)
                                          (hl:tl)    = group l
@

== Exemplos de utilização:
>>> validaComp2 (Mapa 4 [(Rio 1, [Tronco, Tronco, Nenhum, Tronco]),(Carro (-2), [Carro, Nenhum, Carro, Carro])])
True

>>> validaComp2 (Mapa 7 [(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])])
False

>>> validaComp2 (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada 1, [Carro, Carro, Nenhum, Nenhum, Carro])])
True
-}

validaComp2 :: Mapa -> Bool
validaComp2 (Mapa _ []) = True
validaComp2 (Mapa l ((_,[]):t)) = validaComp (Mapa l t)
validaComp2 mapa@(Mapa _ ((e,lT):t)) | head lT == Carro  = if all (== Carro) lT  || nElementos > 3 then False else validaComp1 mapa
                                     | head lT == Tronco = if all (== Tronco) lT || nElementos > 5 then False else validaComp1 mapa
                                     | otherwise         = validaComp1 mapa
                                     where nElementos = length hl + length (last tl)
                                           (hl:tl)    = group lT

{-| A função 'validaCaminho' recebe um mapa e verifica se em cada par __(Terreno,[Obstáculo])__ existe a ausência
de pelo menos um obstáculo, ou seja, averigua se em cada terreno existe um "obstáculo" __Nenhum__, havendo assim pelo menos um
espaço livre. Com a exceção do __Rio__ quando a largura do mapa é superior a 1, onde têm que haver pelo menos um __Tronco__ e um __Nenhum__, visto que, caso não haja, o Jogador não consegue continuar o jogo.
Assim, caso exista em cada terreno um obstáculo __Nenhum__, a função devolve True, caso contrário devolve False.

A função poderia ser definida da seguinte forma:

@
validaCaminho :: Mapa -> Bool
validaCaminho (Mapa _ []) = True
validaCaminho (Mapa l ((Rio _,lT):t)) | l == 1 && Nenhum `elem` lT                     = validaCaminho (Mapa l t)
                                      | l > 1  && Tronco `elem` lT && Nenhum `elem` lT = validaCaminho (Mapa l t)
                                      | otherwise = False
validaCaminho (Mapa l ((e,lT):t)) = if Nenhum `elem` lT then validaCaminho (Mapa l t) else False
@ 

== Exemplos de utilização:

>>> validaCaminho (Mapa 3 [(Rio 2, [Nenhum, Tronco, Nenhum]),(Estrada 2, [Carro, Carro, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum])])
True

>>> validaCaminho (Mapa 3 [(Rio 2, [Nenhum, Tronco, Nenhum]),(Estrada 2, [Carro, Carro, Nenhum]),(Relva, [Arvore, Arvore, Arvore])])
False
-}

validaCaminho :: Mapa         -- ^ argumento: um mapa 
                      -> Bool -- ^ resultado: True caso haja "Nenhum", caso contrário False
validaCaminho (Mapa _ []) = True
validaCaminho (Mapa l ((Rio _,lT):t)) | l == 1 && Nenhum `elem` lT                     = validaCaminho (Mapa l t)
                                      | l >  1 && Tronco `elem` lT && Nenhum `elem` lT = validaCaminho (Mapa l t)
                                      | otherwise = False
validaCaminho (Mapa l ((e,lT):t)) = if Nenhum `elem` lT then validaCaminho (Mapa l t) else False


{-| A função 'validaTamanho' recebe um mapa e analisa se a lista de obstáculos de cada terreno tem a mesma largura
que o mapa.
Deste modo, caso o tamanho de uma lista de obstáculos de um qualquer terreno for superior ou inferior à largura 
do mapa, a função retorna False, caso seja igual, retorna True.

A função poderia ser definida da seguinte forma:

@
validaTamanho :: Mapa -> Bool
validaTamanho (Mapa _ []) = True
validaTamanho (Mapa l ((e,lT):t)) = if length lT == l then validaTamanho (Mapa l t) else False
@ 

== Exemplos de utilização:

>>> validaTamanho (Mapa 3 [(Rio 2, [Nenhum, Tronco, Nenhum]),(Estrada 2, [Carro, Carro, Nenhum]),(Relva, [Arvore, Arvore, Arvore])])
True

>>> validaTamanho (Mapa 3 [(Rio 2, [Nenhum, Tronco, Nenhum]),(Estrada 2, [Carro, Carro, Nenhum]),(Relva, [Arvore, Arvore])])
False
-}

validaTamanho :: Mapa         -- ^ argumento: um mapa 
                      -> Bool -- ^ resultado: True caso o tamanho de cada lista de obstáculos de cada terreno seja igual à largura do mapa, caso contrário False
validaTamanho (Mapa _ []) = True
validaTamanho (Mapa l ((e,lT):t)) = if length lT == l then validaTamanho (Mapa l t) else False

{-| A função 'validaSeq' recebe um mapa e analisa se este não contém mais do que 4 rios seguidos, nem 5 estradas 
ou relvas seguidas.
De forma que, se o mapa receber até __4__ __Rios__ ou __5__ __Estradas__ ou __5__ __Relvas__, a função retorna True, 
caso contrário, retorna False.

A função poderia ser definida da seguinte forma:

@
validaSeq :: Mapa -> Bool
validaSeq (Mapa _ []) = True
validaSeq (Mapa l lT)
   | all (==("Rio")) (take 5 terrenos) && length terrenos > 4     = False
   | all (==("Estrada")) (take 6 terrenos) && length terrenos > 5 = False
   | all (=="Relva") (take 6 terrenos) && length terrenos > 5     = False
   | otherwise = validaSeq (Mapa l (tail lT))
   where vel :: Mapa -> [String]
         vel (Mapa _ []) = []
         vel (Mapa l ((e,lT):t)) = case e of Estrada _ -> "Estrada":vel (Mapa l t)
                                             Rio _     -> "Rio"    :vel (Mapa l t)
                                             Relva     -> "Relva"  :vel (Mapa l t)
         terrenos = vel (Mapa l lT)
@ 

== Exemplos de utilização:

>>> validaSeq (Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum])])
True

>>> validaSeq (Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum])])
False
-}

validaSeq :: Mapa        -- ^ argumento: um mapa 
                 -> Bool -- ^ resultado: True caso não haja mais do que 4 rios ou 5 estradas ou 5 relvas seguidos, caso contrário False
validaSeq (Mapa _ []) = True
validaSeq (Mapa l lT)
   | all (==("Rio")) (take 5 terrenos) && length terrenos > 4     = False
   | all (==("Estrada")) (take 6 terrenos) && length terrenos > 5 = False
   | all (=="Relva") (take 6 terrenos) && length terrenos > 5     = False
   | otherwise = validaSeq (Mapa l (tail lT))
   where vel :: Mapa -> [String]
         vel (Mapa _ []) = []
         vel (Mapa l ((e,lT):t)) = case e of Estrada _ -> "Estrada":vel (Mapa l t)
                                             Rio _     -> "Rio"    :vel (Mapa l t)
                                             Relva     -> "Relva"  :vel (Mapa l t)
         terrenos = vel (Mapa l lT)

{-| A função 'validaArvores' recebe um mapa e analisa se este não arvores impossiveis.

A função poderia ser definida da seguinte forma:

@
validaArvores :: Mapa -> Bool
validaArvores (Mapa _ []) = True
validaArvores (Mapa l ((Relva,obst1):(Relva,obst2):t)) = elem (Nenhum,Nenhum) (zip obst1 obst2) && validaArvores (Mapa l ((Relva,obst2):t))
validaArvores (Mapa l (h:t)) = validaArvores (Mapa l t)
@ 

== Exemplos de utilização:

>>> validaArvores (Mapa 4 [(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco])])
True

>>> validaArvores (Mapa 4 [(Estrada 1,[Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco])])
True
-}

validaArvores :: Mapa -> Bool
validaArvores (Mapa _ []) = True
validaArvores (Mapa l ((Relva,obst1):(Relva,obst2):t)) = elem (Nenhum,Nenhum) (zip obst1 obst2) && validaArvores (Mapa l ((Relva,obst2):t))
validaArvores (Mapa l (h:t)) = validaArvores (Mapa l t)