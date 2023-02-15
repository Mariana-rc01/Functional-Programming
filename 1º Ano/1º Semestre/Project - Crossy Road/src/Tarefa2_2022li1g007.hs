{- |
Module      : Tarefa2_2022li1g007
Description : Geração contínua de um mapa
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}

module Tarefa2_2022li1g007 where

import LI12223
import Data.List
import System.Random

{-| A função 'estendeMapa' __gere__ e __adiciona__ __uma__ __nova__ __linha__ __válida__ ao topo do mapa dado. Para tal, a função recebe um mapa válido e um inteiro.
O parâmetro do tipo __Int__ é um inteiro aleatório (no intervalo [0,100]) que concede alguma pseudo-aleatoriedade à geração da nova linha.

A 'estendeMapa' verifica quais os terrenos passíveis de serem usados na nova linha do mapa e escolhe um terreno com o auxílio da seed (inteiro recebido).

Para definir velocidade para o terreno gerado, seguimos um raciocínio similar ao usado para o terreno gerado, com o auxílio da função 'determinaVelocidade'.
Caso o terreno gerado seja Rela«va, não lhe é atribuída velocidade.

Com o auxílio da função 'proxObs' e 'proximasArvores', a função 'estendeMapa' gera uma linha de obstáculos passíveis de serem usados na linha gerada. 

A função poderia ser definida da seguinte forma:

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa mapa@(Mapa l lT@((ter,obst):ts)) seed  | terreno == Relva && ter == Relva                = Mapa l ((Relva, arvores):lT)
                                                   | terreno == Relva                                = Mapa l ((Relva, obstaculos):lT)
                                                   | terreno == Rio 0 && (fst $ string ter) == "Rio" = Mapa l ((Rio vR, obstaculos):lT)
                                                   | terreno == Rio 0                                = Mapa l ((Rio v, obstaculos):lT)
                                                   | terreno == Estrada 0                            = Mapa l ((Estrada v, obstaculos):lT)
  where terreno    = terrenos !! (mod seed (length terrenos))
        terrenos   = proximosTerrenosValidos mapa
        v          = determinaVelocidade l seed
        vR         = determinaVelocidadeRio (snd $ string ter) l seed
        obstaculos = proxObs (mkStdGen seed) l (terreno, [])
        arvores    = proximasArvores (mkStdGen seed) [(terreno,obstaculos),(ter,obst)]
        string terreno = case terreno of Rio v     -> ("Rio",v)
                                         Estrada v -> ("Estrada",v)
                                         Relva     -> ("Relva",0)  
@

== Exemplos de utilização:

>>> estendeMapa (Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum]),(Rio (-2),[Tronco,Nenhum])]) 75
Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio 2,[Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio (-2),[Tronco,Nenhum])]

>>> estendeMapa (Mapa 3 [(Estrada 1,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Rio (-2), [Nenhum,Tronco,Tronco])]) 0
Mapa 3 [(Rio (-1),[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Rio (-2),[Nenhum,Tronco,Tronco])]
-}

estendeMapa :: Mapa                 -- ^ 1º argumento: um mapa
                    -> Int          -- ^ 2º argumento: número inteiro entre 0 e 100 (seed)
                            -> Mapa -- ^ resultado: o mapa fornecido com a linha gerada                                                   
estendeMapa mapa@(Mapa l lT@((ter,obst):ts)) seed  | terreno == Relva && ter == Relva                = Mapa l ((Relva, arvores):lT)
                                                   | terreno == Relva                                = Mapa l ((Relva, obstaculos):lT)
                                                   | terreno == Rio 0 && (fst $ string ter) == "Rio" = Mapa l ((Rio vR, obstaculos):lT)
                                                   | terreno == Rio 0                                = Mapa l ((Rio v, obstaculos):lT)
                                                   | terreno == Estrada 0                            = Mapa l ((Estrada v, obstaculos):lT)
  where terreno    = terrenos !! (mod seed (length terrenos))
        terrenos   = proximosTerrenosValidos mapa
        v          = determinaVelocidade l seed
        vR         = determinaVelocidadeRio (snd $ string ter) l seed
        obstaculos = proxObs (mkStdGen seed) l (terreno, [])
        arvores    = proximasArvores (mkStdGen seed) [(terreno,obstaculos),(ter,obst)]
        string terreno = case terreno of Rio v     -> ("Rio",v)
                                         Estrada v -> ("Estrada",v)
                                         Relva     -> ("Relva",0) 

{-| A função 'determinaVelocidadeRio' atribui uma __velocidade__ aos __Rios__, caso haja um __Rio__ a anteceder, tendo em conta que se a largura do mapa for inferior a 3, 
conferindo uma velocidade entre -1 e 1, caso contrário concede uma velocidade entre -3 e 3. 

Esta função não atribui velocidades nulas.

A função poderia ser definida da seguinte forma:

@
determinaVelocidadeRio :: Int -> Int -> Int -> Int
determinaVelocidadeRio v l seed | v > 0  = -1
                                | v < 0  =  1
@

== Exemplos de utilização:

>>> determinaVelocidade 2 4 1
-1

>>> determinaVelocidade (-2) 2 40
1
-}

determinaVelocidadeRio :: Int                        -- ^ 1º argumento: velocidade do __Rio__ anterior
                              -> Int                 -- ^ 2º argumento: largura do mapa
                                     -> Int          -- ^ 3º argumento: número aleatório entre 0 e 100
                                           -> Int    -- ^ resultado: velocidade
determinaVelocidadeRio v l seed | v > 0  = -1
                                | v < 0  =  1

{-| A função 'determinaVelocidade' atribui uma __velocidade__, tendo em conta que se a largura do mapa for inferior a 3, 
conferindo uma velocidade entre -1 e 1, caso contrário concede uma velocidade entre -3 e 3.

Esta função não atribui velocidades nulas.

A função poderia ser definida da seguinte forma:

@
determinaVelocidade :: Int -> Int -> Int
determinaVelocidade l seed | l < 3     = (delete 0 [-1..1]) !! (mod (head(geraAleatorios seed 2)) 2)
                           | otherwise = (delete 0 [-3..3]) !! (mod (head(geraAleatorios seed 6)) 6)
                           where geraAleatorios :: Int -> Int -> [Int]
                                 geraAleatorios s c = take c $ randoms (mkStdGen s)
@

== Exemplos de utilização:

>>> determinaVelocidade 3 0
-1

>>> determinaVelocidade 2 75
1
-}

determinaVelocidade :: Int                 -- ^ 1º argumento: largura do mapa
                           -> Int          -- ^ 2º argumento: número aleatório entre 0 e 100
                                    -> Int -- ^ resultado: velocidade
determinaVelocidade l seed | l < 4     = (delete 0 [-1..1]) !! (mod (head(geraAleatorios seed 2)) 2)
                           | otherwise = (delete 0 [-3..3]) !! (mod (head(geraAleatorios seed 6)) 6)
                           where geraAleatorios :: Int -> Int -> [Int]
                                 geraAleatorios s c = take c $ randoms (mkStdGen s)

{-| A função 'proxObs' tem como objetivo gerar uma lista de obstáculos para um terreno de forma que o 
tamanho dessa lista não seja superior à largura do mapa.

Desta forma, a função verifica quais os obstáculos passíveis de serem usados no par __(Terreno,[obstáculos])__ e adiciona à lista de obstáculos
um deles numa forma aleatória.

Quando a lista de obstáculos tiver um tamanho igual à largura do mapa, a função devolve a lista dos obstáculos.

A função poderia ser definida da seguinte forma:

@
proxObs :: StdGen -> Int -> (Terreno,[Obstaculo]) -> [Obstaculo]
proxObs seed l (t,lT) | l == length lT = lT
                      | otherwise      = proxObs proximoAleatorio l (t,[obst]++lT)
                    where listaObst = proximosObstaculosValidos l (t,lT)
                          obst      = listaObst !! (mod aleatorio (length listaObst))
                          (aleatorio,proximoAleatorio) = next seed
@

== Exemplos de utilização:

>>> proxObs (mkStdGen 75) 2 (Relva, [])
[Arvore,Nenhum]

>>> proxObs (mkStdGen 0) 10 (Rio (-2),[])
[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]

>>> proxObs (mkStdGen 27) 8 (Estrada 1,[])
[Carro,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum]
-}

proxObs :: StdGen                                                 -- ^ 1º argumento: número aleatório
                  -> Int                                          -- ^ 2º argumento: largura do mapa
                          -> (Terreno,[Obstaculo])                -- ^ 3º argumento: par do terreno gerado e lista de obstaculos que o constitui
                                                  -> [Obstaculo]  -- ^ resultado: lista de obstaculos do terreno gerado com tamanho igual à largura do mapa
proxObs seed l (t,lT) | l == length lT = lT
                      | otherwise      = proxObs proximoAleatorio l (t,[obst]++lT)
                    where listaObst = proximosObstaculosValidos l (t,lT)
                          obst      = listaObst !! (mod aleatorio (length listaObst))
                          (aleatorio,proximoAleatorio) = next seed

{-| A função 'replace' tem como objetivo substituir uns obstáculos numa lista de obstáculos dada umas posiçoes e dado o novo Obstaculo.

A função poderia ser definida da seguinte forma:

@
replace :: Obstaculo -> [Obstaculo] -> [Int] -> [Obstaculo]
replace _ [] _ = []
replace _ l [] = l
replace novoElemento l (posicao:t) = replace novoElemento (take posicao l ++ [novoElemento] ++ drop (posicao+1) l) t
@

== Exemplos de utilização:

>>> replace Nenhum [Nenhum,Arvore,Arvore,Nenhum,Arvore] [3]
[Nenhum,Arvore,Arvore,Nenhum,Arvore]
-}

replace :: Obstaculo                                    -- ^ 1º argumento: obstáculo
                    -> [Obstaculo]                      -- ^ 2º argumento: lista de obstáculos
                                   -> [Int]             -- ^ 3º argumento: lista de posiçoes
                                         -> [Obstaculo] -- ^ resultado: lista de obstáculos com o obstaculo substituido
replace _ [] _ = []
replace _ l [] = l
replace novoElemento l (posicao:t) = replace novoElemento (take posicao l ++ [novoElemento] ++ drop (posicao+1) l) t

{-| A função 'proximasArvores' tem como objetivo ver em que posiçoes existe Nenhum na Relva existente e com o auxilio da funçao auxiliar 'substituiRandom'
substituir alguns obstáculos na Relva criada por Nenhum, de modo que sejam substituidos em algumas posiçoes em que existe Nenhum na Relva existente.

A função poderia ser definida da seguinte forma:

@
proximasArvores :: StdGen -> [(Terreno,[Obstaculo])] -> [Obstaculo]
proximasArvores seed [(Relva,obst1),(Relva,obst2)] = substituiRandom posicoes obst1
                                            where nenhum = elemIndices Nenhum obst2
                                                  posicoes = drop (mod (fst $ next seed) (length nenhum)) nenhum
@

== Exemplos de utilização:

>>> proximasArvores (mkStdGen 90) [(Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]
[Arvore,Arvore,Arvore,Nenhum,Arvore,Nenhum]
-}

proximasArvores :: StdGen                                          -- ^ 1º argumento: número aleatório
                         -> [(Terreno,[Obstaculo])]                -- ^ 2º argumento: lista da Relva criada com a Relva existente
                                                    -> [Obstaculo] -- ^ resultado: lista de obstaculos da Relva criada
proximasArvores seed [(Relva,obst1),(Relva,obst2)] = replace Nenhum obst1 posicoes
                                            where nenhum = elemIndices Nenhum obst2
                                                  posicoes = drop (mod (fst $ next seed) (length nenhum)) nenhum


{-| A função 'proximosTerrenosValidos' recebe um mapa e gera a lista de terrenos possíveis de serem usados numa nova linha no topo do mapa gerado.

As restrições que esta impõem são:

- Não pode haver num mapa mais que 4 rios seguidos

- Não pode haver num mapa mais que 5 estradas seguidas

- Não pode haver num mapa mais que 5 relvas seguidas

Assim, por exemplo, quando ocorre numa lista 4 rios seguidos esta função retornará [Estrada 0,Relva] já que não é possivel ocorrer outro Rio.

A função poderia ser definida da seguinte forma:

@
proximosTerrenosValidos :: Mapa -> [Terreno]

proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]

proximosTerrenosValidos (Mapa _ ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):_))                               = [Estrada 0,Relva] 
proximosTerrenosValidos (Mapa _ ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):_)) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa _ ((Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):_))                = [Estrada 0,Rio 0]

proximosTerrenosValidos (Mapa _ l) = [Rio 0,Estrada 0,Relva]
@

== Exemplos de utilização:

>>> proximosTerrenosValidos (Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2),[Tronco,Nenhum]),(Rio 1, [Tronco,Nenhum]),(Rio (-2),[Tronco,Nenhum])])
[Estrada 0,Relva]

>>> proximosTerrenosValidos (Mapa 2 [(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Estrada 1,[Nenhum,Carro]),(Estrada 1,[Nenhum,Carro]),(Estrada (-1),[Carro,Nenhum])])
[Rio 0,Relva]
-}

proximosTerrenosValidos :: Mapa              -- ^ argumento: um mapa
                               -> [Terreno]  -- ^ resultado: lista de próximos terrenos válidos

proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]

proximosTerrenosValidos (Mapa _ ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):_))                               = [Estrada 0,Relva] 
proximosTerrenosValidos (Mapa _ ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):_)) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa _ ((Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):_))                = [Rio 0,Estrada 0]

proximosTerrenosValidos (Mapa _ l) = [Rio 0,Estrada 0,Relva]

{-| A função 'proximosObstaculosValidos' gera uma lista de obstáculos passíveis de serem utilizados numa dada linha do mapa.

Para isso, ela recebe um inteiro, correspondente à largura do mapa, e um par de um terreno com uma lista de obstáculos. 
Através desses valores devolve uma lista com os próximos obstáculos possíveis.
Esta impõem algumas restrições como:

- O próximo obstáculo de um __Rio__ pertence a __[Nenhum, Tronco]__

- O próximo obstáculo de uma __Estrada__ pertence a __[Nenhum, Carro]__

- O próximo obstáculo de uma __Relva__ pertence a __[Nenhum, Arvore]__

Mais restrições estão impostas pela função auxiliar 'auxObstaculosValidos'.

A função poderia ser definida da seguinte forma:

@
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]

proximosObstaculosValidos _ (Rio _, [])     = [Nenhum, Tronco]
proximosObstaculosValidos _ (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos _ (Relva, [])     = [Nenhum, Arvore]


proximosObstaculosValidos l (t, lT) = if l == length lT then []
                                      else auxObstaculosValidos l (t,lT)
@

== Exemplos de utilização:

>>> proximosObstaculosValidos 6 (Rio (-1), [Nenhum,Nenhum])
[Tronco]

>>> proximosObstaculosValidos 6 (Estrada (-1), [Nenhum,Nenhum])
[Nenhum, Carro]

-}

proximosObstaculosValidos :: Int                                          -- ^ 1º argumento: largura do mapa
                                -> (Terreno, [Obstaculo])                 -- ^ 2º argumento: um par de um terreno com uma lista de obstáculos
                                                         -> [Obstaculo]   -- ^ resultado: lista dos próximos obstáculos válidos

proximosObstaculosValidos _ (Rio _, [])     = [Nenhum, Tronco]
proximosObstaculosValidos _ (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos _ (Relva, [])     = [Nenhum, Arvore]

proximosObstaculosValidos l (t, lT) = if l == length lT then [] else auxObstaculosValidos l (t,lT)

{-| A função 'auxObstaculosValidos' é uma função auxiliar da função 'proximosObstaculosValidos'.
Esta função filtra casos particulares da função 'proximosObstaculosValidos'.
Deste modo, esta filtra os casos em que:

- Há numa lista 5 Troncos seguidos e devolve [Nenhum] já que não podem haver mais do que 5 seguidos

- Há numa lista 3 Carros seguidos e devolve [Nenhum] já que não podem haver mais do que 3 seguidos

- Numa lista do terreno Rio, caso ela não contenha Tronco, devolve [Tronco] 

- Numa lista em que ainda não ocorre Nenhum e devolve [Nenhum]

A função poderia ser definida da seguinte forma:

@
auxObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
auxObstaculosValidos _ (Rio _, (Tronco:Tronco:Tronco:Tronco:Tronco:_)) = [Nenhum]
auxObstaculosValidos _ (Estrada _, (Carro:Carro:Carro:_))              = [Nenhum]

auxObstaculosValidos _ (Rio _, lT) | not(elem Nenhum lT)                                        = [Nenhum]
                                   | not(elem Tronco lT)                                        = [Tronco]
                                   | head lT == last lT && head lT == Tronco && nElementos >= 5 = [Nenhum]
                                   | otherwise                                                  = [Nenhum,Tronco]
                                   where nElementos = length hl + length (last tl)
                                         (hl:tl)    = group lT

auxObstaculosValidos l (Estrada _,lT) | length lT == 1 && not (elem Nenhum lT)                    = [Nenhum]
                                      | head lT == last lT && head lT == Carro && nElementos >= 3 = [Nenhum]
                                      | not (elem Nenhum lT) && (l == (length lT - 1))            = [Nenhum]
                                      | otherwise                                                 = [Nenhum,Carro]
                                      where nElementos = length hl + length (last tl)
                                            (hl:tl)    = group lT

auxObstaculosValidos _ (Relva, lT) = if not(elem Nenhum lT) then [Nenhum] else [Arvore,Nenhum]
@

== Exemplo:

>>> auxObstaculosValidos 7 (Rio (-1), [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])
[Nenhum]

>>> auxObstaculosValidos 5 (Estrada (-1), [Carro,Carro,Nenhum,Carro])
[Nenhum]

>>> auxObstaculosValidos 4 (Rio (-2), [Nenhum,Nenhum,Nenhum])
[Tronco]

-}
auxObstaculosValidos :: Int                                           -- ^ 1º argumento: largura do mapa
                            -> (Terreno, [Obstaculo])                 -- ^ 2º argumento: um par de um terreno com uma lista de obstáculos
                                                      -> [Obstaculo]  -- ^ resultado: lista de obstáculos passíveis de serem usados com algumas restrições
auxObstaculosValidos _ (Rio _, (Tronco:Tronco:Tronco:Tronco:Tronco:_)) = [Nenhum]
auxObstaculosValidos _ (Estrada _, (Carro:Carro:Carro:_))              = [Nenhum]

auxObstaculosValidos _ (Rio _, lT) | not(elem Nenhum lT)                                        = [Nenhum]
                                   | not(elem Tronco lT)                                        = [Tronco]
                                   | head lT == last lT && head lT == Tronco && nElementos >= 5 = [Nenhum]
                                   | otherwise                                                  = [Nenhum,Tronco]
                                   where nElementos = if tl == [] then length hl else length hl + length (last tl)
                                         (hl:tl)    = group lT

auxObstaculosValidos l (Estrada _,lT) | length lT == 1 && not (elem Nenhum lT)                    = [Nenhum]
                                      | not (elem Nenhum lT) && (l == (length lT + 1))            = [Nenhum]
                                      | head lT == last lT && head lT == Carro && nElementos >= 3 = [Nenhum]
                                      | otherwise                                                 = [Nenhum,Carro]
                                      where nElementos = if tl == [] then length hl else length hl + length (last tl)
                                            (hl:tl)    = group lT

auxObstaculosValidos _ (Relva, lT) = if not(elem Nenhum lT) then [Nenhum] else [Arvore,Nenhum]
