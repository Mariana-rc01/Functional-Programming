{- |
Module      : LI12223
Description : Módulo auxiliar para LI1 22/23.
Copyright   : Manuel Barros <d13242@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2022/23.
 -}
module LI12223 (
  -- * Tipos de dados
  -- ** Básicos
  Coordenadas , Largura , Velocidade,
  -- ** Mapas
  Mapa(..), Terreno(..), Obstaculo(..),
    -- ** Jogo
  Jogo(..), Jogador(..), Direcao(..), Jogada(..),
    -- ** Jogo MultiPlayer
  JogoM(..),
   -- ** Gloss
  Imagem, Fundo, Imagens, MapaString, Skins, Pontuacao, EstadoGloss,
  Opcao(..), Pausa(..), Perdeu(..), Modo(..), Menu(..)
  ) where

import Graphics.Gloss

-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Show, Read, Eq)

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos em 'Rio'
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Show, Read, Eq)

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | O Jogador define o personagem controlado no 'Jogo'.
newtype Jogador =
  Jogador Coordenadas
  deriving (Show, Read, Eq)

-- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)

-- | Tipos de dados extras para a realização do modo MultiPlayer.

-- | Definição base de um jogo multiplayer.
data JogoM =
  JogoM
    Jogador -- ^ Uma personagem do jogo multiplayer
    Jogador -- ^ Outra personagem do jogo multiplayer
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Tipos de dados auxiliares para a realização da Interface gráfica do Jogo

-- | Imagem associada a cada 'Terreno' e a cada 'Obstaculo'.
type Imagem = [(MapaString,(Picture, (Float,Float)))]

-- | Imagens associadas a cada 'Terreno' e a cada 'Obstaculo'.
type Imagens = [Imagem]

-- | Imagens do Menu e Pontuação.
type Fundo = [(String,Picture)]

-- | 'MapaString' é a String associada a cada 'Terreno' e 'Obstaculo'.
type MapaString = String

-- | Lista de imagens que o 'Jogador' pode tomar.
type Skins            = [(Jogador,String,Picture)]

-- | Pontuação do 'Jogador'.
type Pontuacao        = Int

-- | 'EstadoGloss' é o Estado em que o 'Jogo' se encontra no Gloss.
type EstadoGloss      = (Menu, Jogo, JogoM, Imagens, Fundo, Float, Skins, [Pontuacao])

-- | 'Opcao' são as opções que aparecem no Menu Principal.
data Opcao = Jogar
            | Sair
            | Instrucoes deriving (Eq)

-- | 'Perdeu' são as opções que aparecem quando o Jogador perde.
data Perdeu =  Jogo1   | Jogo2   | Jogo3 
             | Menu1   | Menu2   | Menu3
             | JogoM11 | JogoM12 | JogoM21 | JogoM22 | JogoM31 | JogoM32 
             | MenuM11 | MenuM12 | MenuM21 | MenuM22 | MenuM31 | MenuM32
             | EmpataramJ1 | EmpataramJ2 | EmpataramJ3
             | EmpataramS1 | EmpataramS2 | EmpataramS3
            deriving (Eq)

-- | 'Pausa' são as opções que aparecem quando o Jogador pausa o Jogo.
data Pausa = Retomar1 | Retomar2 | Retomar3 
           | RetomarM1| RetomarM2| RetomarM3
           | Sair1    | Sair2    | Sair3    
           | SairM1   | SairM2   | SairM3 deriving (Eq)

-- | 'Modo' são as opções que o Jogador tem, isto é, pode escolher jogar sozinho ou com mais um jogador.
data Modo = Single | Multi | Voltar deriving (Eq)

-- | 'Menu' contem as diferentes opções de interação que o jogador pode ter para jogar.
data Menu = Opcoes Opcao
          | Perdeu Perdeu
          | Pausa Pausa
          | Modo Modo
          | Bot1          | Bot2          | Bot3
          | Inicio1       | Inicio2       | Inicio3     
          | InicioM1      | InicioM2      | InicioM3
          | ModoJogo1     | ModoJogo2     | ModoJogo3   
          | ModoJogoM1    | ModoJogoM2    | ModoJogoM3 
          | Personagem1   | Personagem2   | Personagem3 | PersonagemVoltar
          | PersonagemM1  | PersonagemM2  | PersonagemM3| PersonagemVoltarM
          | ModoInstrucoes1 | ModoInstrucoesC | ModoInstrucoesS
           deriving (Eq)