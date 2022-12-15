module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Bifunctor

type Posicao = (Int,Int)
type Maca = Posicao
type Cobra = Posicao
data Jogo = Jogo Cobra [Maca]

data Jogada = Cima | Baixo | Esquerda | Direita | Parado
type Estado = Jogo
type World = Jogo

estadoInicial :: World
estadoInicial = Jogo (200,0) [(50,100),(200,-400),(-400,-200)]

fromInts :: (Int,Int) -> (Float,Float)
fromInts = bimap fromIntegral fromIntegral

desenha :: Jogo -> Picture
desenha (Jogo cobra macas) = Pictures $ (desenhacobra cobra): map desenhamaca macas

desenhacobra :: Cobra -> Picture
desenhacobra cobra = Translate x y $ Color green $ circleSolid 20
                 where (x, y) = fromInts cobra

desenhamaca :: Maca -> Picture
desenhamaca maca = Translate x y $ Color red $ circleSolid 10
                 where (x,y) = fromInts maca

evento :: Event -> Estado -> Estado
evento (EventKey (SpecialKey KeyUp) Down _ _)    (Jogo (x,y) macas) = (Jogo (x,y+10) macas) 
evento (EventKey (SpecialKey KeyDown) Down _ _)  (Jogo (x,y) macas) = (Jogo (x,y-10) macas)
evento (EventKey (SpecialKey KeyLeft) Down _ _)  (Jogo (x,y) macas) = (Jogo (x-10,y) macas)
evento (EventKey (SpecialKey KeyRight) Down _ _) (Jogo (x,y) macas) = (Jogo (x+10,y) macas) -- Down quer dizer que a tecla foi clicada
evento _ e = e

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (x,y) macas) Cima = (Jogo (x,y+10) (filter (/= (x,y+10)) macas))
animaJogo (Jogo (x,y) macas) Baixo = (Jogo (x,y-10) (filter (/= (x,y-10)) macas))
animaJogo (Jogo (x,y) macas) Direita = (Jogo (x+10,y) (filter (/= (x+10,y)) macas))
animaJogo (Jogo (x,y) macas) Esquerda = (Jogo (x-10,y) (filter (/= (x-10,y)) macas))
animaJogo (Jogo (x,y) macas) Parado = (Jogo (x,y) (filter (/= (x,y)) macas))

tempo :: Float -> Estado -> Estado
tempo _ e = e

fr :: Int
fr = 15

janela :: Display
janela = InWindow "Snake da Wish" (800,800) (0,0)

main = play janela black fr estadoInicial desenha evento tempo