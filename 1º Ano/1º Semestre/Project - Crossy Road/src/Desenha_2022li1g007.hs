{- |
Module      : Desenha_2022li1g007
Description : Desenha o Jogo (Gloss)
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module Desenha_2022li1g007 where

import LI12223
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Data.List

altura :: Float
altura = 450

comprimento :: Float
comprimento = (-500)

fr :: Int
fr = 1

dm :: Display
dm = InWindow "Crossy Road" (2560, 2000) (0,0)

l :: Float
l = 75

-- -- Desenhar Mapa:

obstaculos :: Jogo -> [(Terreno,[[Obstaculo]])]
obstaculos (Jogo _ (Mapa _ lT)) = map (\(x,l) -> (x,group l)) lT

obstaculosM :: JogoM -> [(Terreno,[[Obstaculo]])]
obstaculosM (JogoM _ _ (Mapa _ lT)) = map (\(x,l) -> (x,group l)) lT

obstaculoStr :: (Terreno,[[Obstaculo]]) -> [String]
obstaculoStr (_,[]) = []
obstaculoStr (Estrada v, obst:os) | v > 0 && l == 1 = if (head obst) == Nenhum then "Nenhum" :obstaculoStr (Estrada v,os) else "Carro" :obstaculoStr (Estrada v,os)
                                  | v < 0 && l == 1 = if (head obst) == Nenhum then "Nenhum" :obstaculoStr (Estrada v,os) else "CarroR":obstaculoStr (Estrada v,os)
                                    
                                  | v > 0 && l == 2 = if (head obst) == Nenhum then (replicate l "Nenhum") ++ obstaculoStr (Estrada v,os) else ["Nenhum","Camiao"]++obstaculoStr (Estrada v,os)
                                  | v < 0 && l == 2 = if (head obst) == Nenhum then (replicate l "Nenhum") ++ obstaculoStr (Estrada v,os) else ["Nenhum","CamiaoR"]++obstaculoStr (Estrada v,os)
                                    
                                  | v > 0 && l == 3 = if (head obst) == Nenhum then (replicate l "Nenhum") ++ obstaculoStr (Estrada v,os) else ["Nenhum","Autocarro","Nenhum"] ++ obstaculoStr (Estrada v,os)
                                  | v < 0 && l == 3 = if (head obst) == Nenhum then (replicate l "Nenhum") ++ obstaculoStr (Estrada v,os) else ["Nenhum","AutocarroR","Nenhum"] ++ obstaculoStr (Estrada v,os)
                                  where l = length obst 

obstaculoStr (t,obst:os) = case (head obst) of Tronco -> (replicate l "Tronco") ++ obstaculoStr (t,os)
                                               Nenhum -> (replicate l "Nenhum") ++ obstaculoStr (t,os)
                                               Arvore -> (replicate l "Arvore") ++ obstaculoStr (t,os)
                            where l = length obst

terrenos :: Mapa -> [[Terreno]]
terrenos (Mapa _ [])               = []
terrenos (Mapa l ((terreno,_):t))  = replicate l terreno: terrenos (Mapa l t)

terrenoStr :: [Terreno] -> [String]
terrenoStr [] = []
terrenoStr (terreno:ts) = case terreno of Estrada _ -> "Estrada": terrenoStr ts
                                          Rio _     -> "Rio"    : terrenoStr ts
                                          Relva     -> "Relva"  : terrenoStr ts

-- Desenha os terrenos:

desenhaTerreno :: Float -> Float -> MapaString -> Imagem -> Picture
desenhaTerreno x y terreno texturaTer = Translate xx yy textura
   where tuple   = (fromJust . lookup terreno) texturaTer
         textura = fst tuple
         xx      = (fst . snd) tuple + x
         yy      = (snd . snd) tuple + y

desenhaLinhaTerreno :: Float -> Float -> [MapaString] -> Imagem -> [Picture]
desenhaLinhaTerreno x y (h:t) texturaTer = terreno : restantesTerrenos
   where terreno           = desenhaTerreno x y h texturaTer
         restantesTerrenos = desenhaLinhaTerreno (x+l) y t texturaTer
desenhaLinhaTerreno _ _ _ _ = []

desenhaTerrenos :: Float -> Float -> [[MapaString]] -> Imagem -> [Picture]
desenhaTerrenos x y (h:t) texturaTer = linha ++ restantesLinhas
    where linha           = desenhaLinhaTerreno x y h texturaTer
          restantesLinhas = desenhaTerrenos x (y-l) t texturaTer
desenhaTerrenos _ _ _ _ = []

-- Desenha os obstáculos:

desenhaObstaculo :: Float -> Float -> MapaString -> Imagem -> Picture
desenhaObstaculo x y obstaculo texturaObst = Translate xx yy textura
   where tuple   = (fromJust . lookup obstaculo) texturaObst
         textura = fst tuple
         xx      = (fst . snd) tuple + x
         yy      = (snd . snd) tuple + y

desenhaLinhaObstaculo :: Float -> Float -> [MapaString] -> Imagem -> [Picture]
desenhaLinhaObstaculo x y (h:t) texturaObst = obstaculo : restantesObstaculos
   where obstaculo           = desenhaObstaculo x y h texturaObst
         restantesObstaculos = desenhaLinhaObstaculo (x+l) y t texturaObst
desenhaLinhaObstaculo _ _ _ _ = []

desenhaObstaculos :: Float -> Float -> [[MapaString]] -> Imagem -> [Picture]
desenhaObstaculos x y (h:t) texturaObst = linha ++ restantesLinhas
    where linha           = desenhaLinhaObstaculo x y h texturaObst
          restantesLinhas = desenhaObstaculos x (y-l) t texturaObst
desenhaObstaculos _ _ _ _ = []

-- -- Desenhar jogador:

novasCoordenadas :: Coordenadas -> (Float,Float)
novasCoordenadas (x,y) = ((fromIntegral x*l) + comprimento, (fromIntegral y*(-l)) + altura)

desenhaJogador :: Jogador -> String -> Skins -> Picture
desenhaJogador (Jogador (x,y)) string skins = Translate xx yy image
  where (xx,yy) = novasCoordenadas (x,y)
        image   = ((fromJust . lookup string) listaSkins)
        listaSkins = map (\(x,y,z) -> (y,z)) skins

-- -- Desenha pontuação:

splitNumero :: String -> [String]
splitNumero n = map (\x -> [x]) n

translacaoDosNumeros :: [Picture] -> Float -> [Picture]
translacaoDosNumeros [] _ = []
translacaoDosNumeros (h:t) acc = Translate acc 0 h : translacaoDosNumeros t (acc+50)

desenhaPontuacao :: Pontuacao -> Fundo -> Picture
desenhaPontuacao n numeros | n < 0 = (fromJust . lookup "0") numeros
                           | otherwise = Pictures $ translacaoDosNumeros numero 0
                         where l = splitNumero $ show n
                               numero = map (\x -> (fromJust . lookup x) numeros) l

-- -- Desenha Jogo:

desenhaEstado :: EstadoGloss -> Picture

-- Menu:
desenhaEstado (Opcoes Jogar     , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "menuJogar") fundo)])
desenhaEstado (Opcoes Sair      , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "menuSair") fundo)])
desenhaEstado (Opcoes Instrucoes, _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "menuInstrucoes") fundo)])

-- Instrucoes:
desenhaEstado (ModoInstrucoes1  , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoInstrucoes1") fundo)])
desenhaEstado (ModoInstrucoesC  , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoInstrucoesC") fundo)])
desenhaEstado (ModoInstrucoesS  , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoInstrucoesS") fundo)])

-- Escolha do Modo de Jogo:
desenhaEstado (Modo Single, _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoSingle") fundo)])
desenhaEstado (Modo Multi , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoMulti") fundo)])
desenhaEstado (Modo Voltar, _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "modoVoltar") fundo)])

-- Escolha da Personagem:

-- SinglePlayer
desenhaEstado (Personagem1     , _, _, _, fundo, _, _, _) =  Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem1") fundo)])
desenhaEstado (Personagem2     , _, _, _, fundo, _, _, _) =  Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem2") fundo)])
desenhaEstado (Personagem3     , _, _, _, fundo, _, _, _) =  Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem3") fundo)])
desenhaEstado (PersonagemVoltar, _, _, _, fundo, _, _, _) =  Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagemVoltar") fundo)])
-- MultiPlayer
desenhaEstado (PersonagemM1     , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem1M") fundo)])
desenhaEstado (PersonagemM2     , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem2M") fundo)])
desenhaEstado (PersonagemM3     , _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagem3M") fundo)])
desenhaEstado (PersonagemVoltarM, _, _, _, fundo, _, _, _) = Pictures $ ([Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)] ++ [Translate (-50) (-100) ((fromJust . lookup "personagemVoltarM") fundo)])

-- Jogo:

desenhaEstado (k ,jogo,jogoM,imagens,fundo,tempo,skins,points) 
  | k == Inicio1  || k == ModoJogo1  || k == Pausa Sair1  || k == Pausa Retomar1  || k == Bot1 || k == Perdeu Jogo1 || k == Perdeu Menu1 = desenhaEstado1  (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)
  | k == Inicio2  || k == ModoJogo2  || k == Pausa Sair2  || k == Pausa Retomar2  || k == Bot2 || k == Perdeu Jogo2 || k == Perdeu Menu2 = desenhaEstado2  (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)
  | k == Inicio3  || k == ModoJogo3  || k == Pausa Sair3  || k == Pausa Retomar3  || k == Bot3 || k == Perdeu Jogo3 || k == Perdeu Menu3 = desenhaEstado3  (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)
  | k == InicioM1 || k == ModoJogoM1 || k == Pausa SairM1 || k == Pausa RetomarM1 || k == Perdeu JogoM11 || k == Perdeu JogoM12 || k == Perdeu MenuM11 || k == Perdeu MenuM12 || k == Perdeu EmpataramJ1 || k == Perdeu EmpataramS1 = desenhaEstadoM1 (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)
  | k == InicioM2 || k == ModoJogoM2 || k == Pausa SairM2 || k == Pausa RetomarM2 || k == Perdeu JogoM21 || k == Perdeu JogoM22 || k == Perdeu MenuM21 || k == Perdeu MenuM22 || k == Perdeu EmpataramJ2 || k == Perdeu EmpataramS2 = desenhaEstadoM2 (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)
  | k == InicioM3 || k == ModoJogoM3 || k == Pausa SairM3 || k == Pausa RetomarM3 || k == Perdeu JogoM31 || k == Perdeu JogoM32 || k == Perdeu MenuM31 || k == Perdeu MenuM32 || k == Perdeu EmpataramJ3 || k == Perdeu EmpataramS3 = desenhaEstadoM3 (k ,jogo,jogoM,imagens,fundo,tempo,skins,points)

-- SinglePlayer1
desenhaEstado1 (k1 ,jogo@(Jogo jogador mapaa),jogoM,(i1:_),fundo,tempo,skins,[pontuacao,_]) = case k1 of Inicio1        -> jogoGloss
                                                                                                         ModoJogo1      -> jogoGloss
                                                                                                         Bot1           -> jogoGloss
                                                                                                         Pausa Sair1    -> jogoPausa
                                                                                                         Pausa Retomar1 -> jogoRetomar
                                                                                                         Perdeu Jogo1   -> jogoPerdeuTentar
                                                                                                         Perdeu Menu1   -> jogoPerdeuSair
    where mapaGloss       = desenhaObstaculos comprimento altura mapa i1
          mapa            = map obstaculoStr (obstaculos jogo)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i1
          jogadorGloss1   = desenhaJogador jogador "Galinha" skins
          points          = Translate 385 (-250) $ desenhaPontuacao pontuacao fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)
          perdeuTentar    = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentar") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          perdeuSair      = [Translate (-50) (-100) ((fromJust . lookup "perdeuSair") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss1])
          jogoPausa       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss1] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar     = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss1] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          jogoPerdeuTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss1] ++ perdeuTentar)
          jogoPerdeuSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss1] ++ perdeuSair)


-- SinglePlayer2
desenhaEstado2 (k2 ,jogo@(Jogo jogador mapaa),jogoM,(_:i2:_),fundo,tempo,skins,[pontuacao,_]) = case k2 of Inicio2        -> jogoGloss
                                                                                                           ModoJogo2      -> jogoGloss
                                                                                                           Bot2           -> jogoGloss
                                                                                                           Pausa Sair2    -> jogoPausa
                                                                                                           Pausa Retomar2 -> jogoRetomar
                                                                                                           Perdeu Jogo2   -> jogoPerdeuTentar
                                                                                                           Perdeu Menu2   -> jogoPerdeuSair
    where mapaGloss       = desenhaObstaculos comprimento altura mapa i2
          mapa            = map obstaculoStr (obstaculos jogo)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i2
          jogadorGloss2   = desenhaJogador jogador "Bolacha" skins
          points          = Translate 385 (-250) $ desenhaPontuacao pontuacao fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo2") fundo)
          perdeuTentar    = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentar") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          perdeuSair      = [Translate (-50) (-100) ((fromJust . lookup "perdeuSair") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss2])
          jogoPausa  = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss2] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar= Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss2] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          jogoPerdeuTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss2] ++ perdeuTentar)
          jogoPerdeuSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss2] ++ perdeuSair)


-- SinglePlayer3
desenhaEstado3 (k3 ,jogo@(Jogo jogador mapaa),jogoM,[_,_,i3],fundo,tempo,skins,[pontuacao,_]) = case k3 of Inicio3        -> jogoGloss
                                                                                                           ModoJogo3      -> jogoGloss
                                                                                                           Bot3           -> jogoGloss
                                                                                                           Pausa Sair3    -> jogoPausa
                                                                                                           Pausa Retomar3 -> jogoRetomar
                                                                                                           Perdeu Jogo3   -> jogoPerdeuTentar
                                                                                                           Perdeu Menu3   -> jogoPerdeuSair
    where mapaGloss       = desenhaObstaculos comprimento altura mapa i3
          mapa            = map obstaculoStr (obstaculos jogo)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i3
          jogadorGloss3   = desenhaJogador jogador "Dragao1" skins
          points          = Translate 385 (-250) $ desenhaPontuacao pontuacao fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo3") fundo)
          perdeuTentar    = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentar") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          perdeuSair      = [Translate (-50) (-100) ((fromJust . lookup "perdeuSair") fundo)] ++ [Translate (-75) (-100) $ desenhaPontuacao pontuacao fundo]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss3])
          jogoPausa       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss3] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar     = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss3] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          jogoPerdeuTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss3] ++ perdeuTentar)
          jogoPerdeuSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogadorGloss3] ++ perdeuSair)


-- MultiPlayer1
desenhaEstadoM1 (kM1 ,jogo,jogoM@(JogoM jogador1 jogador2 mapaa),[i1,i2,_],fundo,tempo,skins,[p1,p2]) = case kM1 of InicioM1           -> jogoGloss
                                                                                                                    ModoJogoM1         -> jogoGloss
                                                                                                                    Pausa SairM1       -> jogoPausa
                                                                                                                    Pausa RetomarM1    -> jogoRetomar
                                                                                                                    Perdeu JogoM11     -> perdeuTentarPato
                                                                                                                    Perdeu MenuM11     -> perdeuSairPato
                                                                                                                    Perdeu JogoM12     -> perdeuTentarGalinha
                                                                                                                    Perdeu MenuM12     -> perdeuSairGalinha
                                                                                                                    Perdeu EmpataramJ1 -> perdeuEmpataramTentar
                                                                                                                    Perdeu EmpataramS1 -> perdeuEmpataramSair

    where mapaGloss       = desenhaObstaculos comprimento altura mapa i1
          mapa            = map obstaculoStr (obstaculosM jogoM)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i1
          jogador1Gloss   = desenhaJogador jogador1 "Pato" skins
          jogador2Gloss   = desenhaJogador jogador2 "Galinha" skins
          points1         = desenhaPontuacao p1 fundo
          points2         = desenhaPontuacao p2 fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo1") fundo)
          tentarPato      = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM11") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairPato        = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM11") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          tentarGalinha   = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM12") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairGalinha     = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM12") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramTentar = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramJ1") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramSair   = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramS1") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss])
          jogoPausa       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar     = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          perdeuTentarPato = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarPato)
          perdeuSairPato   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairPato)
          perdeuTentarGalinha = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarGalinha)
          perdeuSairGalinha   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairGalinha)
          perdeuEmpataramTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramTentar)
          perdeuEmpataramSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramSair)


-- MultiPlayer2
desenhaEstadoM2 (kM2 ,jogo,jogoM@(JogoM jogador1 jogador2 mapaa),[i1,i2,_],fundo,tempo,skins,[p1,p2]) = case kM2 of InicioM2           -> jogoGloss
                                                                                                                    ModoJogoM2         -> jogoGloss
                                                                                                                    Pausa SairM2       -> jogoPausa
                                                                                                                    Pausa RetomarM2    -> jogoRetomar
                                                                                                                    Perdeu JogoM21     -> perdeuTentarBoneco
                                                                                                                    Perdeu MenuM21     -> perdeuSairBoneco
                                                                                                                    Perdeu JogoM22     -> perdeuTentarBolacha
                                                                                                                    Perdeu MenuM22     -> perdeuSairBolacha
                                                                                                                    Perdeu EmpataramJ2 -> perdeuEmpataramTentar
                                                                                                                    Perdeu EmpataramS2 -> perdeuEmpataramSair
    where mapaGloss       = desenhaObstaculos comprimento altura mapa i2
          mapa            = map obstaculoStr (obstaculosM jogoM)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i2
          jogador1Gloss   = desenhaJogador jogador1 "Boneco" skins
          jogador2Gloss   = desenhaJogador jogador2 "Bolacha" skins
          points1         = desenhaPontuacao p1 fundo
          points2         = desenhaPontuacao p2 fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo2") fundo)
          tentarBoneco    = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM21") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairBoneco      = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM21") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          tentarBolacha   = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM22") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairBolacha     = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM22") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramTentar = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramJ2") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramSair   = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramS2") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss])
          jogoPausa       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar     = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          perdeuTentarBoneco = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarBoneco)
          perdeuSairBoneco   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairBoneco)
          perdeuTentarBolacha = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarBolacha)
          perdeuSairBolacha   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairBolacha)
          perdeuEmpataramTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramTentar)
          perdeuEmpataramSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramSair)

-- MultiPlayer3
desenhaEstadoM3 (kM3 ,jogo,jogoM@(JogoM jogador1 jogador2 mapaa),[i1,i2,i3],fundo,tempo,skins,[p1,p2]) = case kM3 of InicioM3           -> jogoGloss
                                                                                                                     ModoJogoM3         -> jogoGloss
                                                                                                                     Pausa SairM3       -> jogoPausa
                                                                                                                     Pausa RetomarM3    -> jogoRetomar
                                                                                                                     Perdeu JogoM31     -> perdeuTentarDragao2
                                                                                                                     Perdeu MenuM31     -> perdeuSairDragao2
                                                                                                                     Perdeu JogoM32     -> perdeuTentarDragao1
                                                                                                                     Perdeu MenuM32     -> perdeuSairDragao1
                                                                                                                     Perdeu EmpataramJ3 -> perdeuEmpataramTentar
                                                                                                                     Perdeu EmpataramS3 -> perdeuEmpataramSair
    where mapaGloss       = desenhaObstaculos comprimento altura mapa i3
          mapa            = map obstaculoStr (obstaculosM jogoM)
          listaTerrenos   = map terrenoStr (terrenos mapaa)
          terrenosGloss   = desenhaTerrenos comprimento altura listaTerrenos i3
          jogador1Gloss   = desenhaJogador jogador1 "Dragao2" skins
          jogador2Gloss   = desenhaJogador jogador2 "Dragao1" skins
          points1         = desenhaPontuacao p1 fundo
          points2         = desenhaPontuacao p2 fundo
          fundoJogo       = Translate 10 (-100) ((fromJust . lookup "modoJogo3") fundo)
          tentarDragao2   = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM31") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairDragao2     = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM31") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          tentarDragao1   = [Translate (-50) (-100) ((fromJust . lookup "perdeuTentarM32") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          sairDragao1     = [Translate (-50) (-100) ((fromJust . lookup "perdeuSairM32") fundo)]   ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramTentar = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramJ3") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          empataramSair   = [Translate (-50) (-100) ((fromJust . lookup "perdeuEmpataramS3") fundo)] ++ [Translate 20 (-150) $ points1, Translate (-385) (-150) $ points2]
          jogoGloss       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss])
          jogoPausa       = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaSair") fundo)])
          jogoRetomar     = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [Translate (-50) (-100) ((fromJust . lookup "pausaRetomar") fundo)])
          perdeuTentarDragao2 = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarDragao2)
          perdeuSairDragao2   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairDragao2)
          perdeuTentarDragao1 = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ tentarDragao1)
          perdeuSairDragao1   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ sairDragao1)
          perdeuEmpataramTentar = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramTentar)
          perdeuEmpataramSair   = Pictures $ ([fundoJogo] ++ terrenosGloss ++ mapaGloss ++ [jogador1Gloss] ++ [jogador2Gloss] ++ [points1,points2] ++ empataramSair)
