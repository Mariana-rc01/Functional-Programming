{- |
Module      : AlteraJogo_2022li1g007
Description : ReageEvento e ReageTempo (Gloss)
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module AlteraJogo_2022li1g007 where

import LI12223
import System.Random
import System.IO.Unsafe
import Graphics.Gloss.Interface.Pure.Game

import Tarefa2_2022li1g007
import Tarefa3_2022li1g007
import Tarefa4_2022li1g007
import Tarefa5_2022li1g007

import MultiPlayerTarefa3_2022li1g007
import MultiPlayerTarefa4e5_2022li1g007

import Bot_2022li1g007

estadoInicial :: Jogo
estadoInicial = (Jogo (Jogador (6,10)) (Mapa 15 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                 (Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                 (Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                 (Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                 (Relva,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore])]))

jogoRandom :: Jogo -> Int -> Jogo
jogoRandom (Jogo jogador mapa) int = (Jogo jogador (mapaF (mkStdGen int) mapa))

mapaF :: StdGen -> Mapa -> Mapa
mapaF seed (Mapa l lT) | length lT == 12 = (Mapa l lT)
                       | otherwise = mapaF proximoAleatorio (estendeMapa (Mapa l lT) aleatorio)
                    where (aleatorio, proximoAleatorio) = randomR (0,100) seed 
                          
estadoInicialM :: JogoM
estadoInicialM = (JogoM (Jogador (8,10)) (Jogador (6,10)) (Mapa 15 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                                    (Relva,[Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
                                                                    (Relva,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore])]))

jogoRandomM :: JogoM -> Int -> JogoM
jogoRandomM (JogoM jogador1 jogador2 mapa) int = (JogoM jogador1 jogador2 (mapaF (mkStdGen int) mapa))


estadoGlossInicial :: Imagens -> Fundo -> Skins -> EstadoGloss
estadoGlossInicial imagens fundo skins = (Opcoes Jogar, jogoRandom estadoInicial 1, jogoRandomM estadoInicialM 1, imagens, fundo, 0, skins, [0,0])


reageEvento :: Event -> EstadoGloss -> EstadoGloss
-- Reage ao Menu Principal:
reageEvento k (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (ModoInstrucoesC  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (ModoInstrucoesC  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = reageMenu k (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- Escolhe como pretende jogar:
reageEvento k (Modo Single      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolheModo k (Modo Single ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Modo Multi       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolheModo k (Modo Multi  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Modo Voltar      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolheModo k (Modo Voltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- Escolher a personagem:
-- SinglePlayer
reageEvento k (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (PersonagemVoltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (PersonagemVoltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
-- MultiPlayer
reageEvento k (PersonagemM1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (PersonagemM1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (PersonagemM2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (PersonagemM2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (PersonagemM3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (PersonagemM3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (PersonagemVoltarM,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = escolhePersonagem k (PersonagemVoltarM ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- Perdeu o Jogo
-- SinglePlayer1
reageEvento k (Perdeu Jogo1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Jogo1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu Menu1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Menu1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer2
reageEvento k (Perdeu Jogo2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Jogo2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu Menu2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Menu2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer3
reageEvento k (Perdeu Jogo3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Jogo3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu Menu3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu Menu3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer1
reageEvento k (Perdeu JogoM11   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM11 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM11   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM11 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu JogoM12   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM12 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM12   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM12 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu EmpataramJ1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramJ1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu EmpataramS1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramS1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer2
reageEvento k (Perdeu JogoM21   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM21 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM21   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM21 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu JogoM22   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM22 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM22   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM22 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu EmpataramJ2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramJ2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu EmpataramS2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramS2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer3
reageEvento k (Perdeu JogoM31   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM31 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM31   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM31 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu JogoM32   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu JogoM32 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu MenuM32   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu MenuM32 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Perdeu EmpataramJ3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramJ3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Perdeu EmpataramS3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = perdeuJogo k (Perdeu EmpataramS3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- Modo Jogo:
-- SinglePlayer1
reageEvento k (Inicio1          ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Inicio1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogo1        ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogo1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Bot1             ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Bot1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer2
reageEvento k (Inicio2          ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Inicio2  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogo2        ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogo2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (Bot2             ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Bot2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer3
reageEvento k (Inicio3          ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Inicio3  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogo3        ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogo3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Bot3             ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (Bot3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer1
reageEvento k (InicioM1         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (InicioM1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogoM1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogoM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
-- MultiPlayer2
reageEvento k (InicioM2         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (InicioM2  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogoM2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogoM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
-- MultiPlayer3
reageEvento k (InicioM3         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (InicioM3  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 
reageEvento k (ModoJogoM3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = modoJogo k (ModoJogoM3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) 

-- Sair da Pausa:
reageEvento k (Pausa Retomar1   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Retomar1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa Sair1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Sair1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Pausa Retomar2   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Retomar2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa Sair2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Sair2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Pausa Retomar3   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Retomar3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa Sair3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa Sair3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Pausa RetomarM1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa RetomarM1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa SairM1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa SairM1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Pausa RetomarM2  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa RetomarM2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa SairM2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa SairM2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageEvento k (Pausa RetomarM3  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa RetomarM3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageEvento k (Pausa SairM3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = pausaJogo k (Pausa SairM3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

--reageEvento _ estado = estado

-----------------------------------------------------------------------------------------------------------------------
reageMenu :: Event -> EstadoGloss -> EstadoGloss
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyUp   ) Down _ _) (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyDown ) Down _ _) (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageMenu (EventKey (SpecialKey KeyUp   ) Down _ _) (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyDown ) Down _ _) (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = error "Fim de Jogo"

reageMenu (EventKey (SpecialKey KeyUp   ) Down _ _) (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyDown ) Down _ _) (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Sair    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Instrucoes,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoes1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageMenu (EventKey (SpecialKey KeyRight) Down _ _) (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoesC,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoesS,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoesS,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (ModoInstrucoes1  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoesC,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoInstrucoesC  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoes1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (ModoInstrucoesC  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoes1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyDown ) Down _ _) (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoes1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyRight) Down _ _) (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoInstrucoes1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
reageMenu (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoInstrucoesS  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

reageMenu _ estado = estado
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
escolheModo :: Event -> EstadoGloss -> EstadoGloss
escolheModo (EventKey (SpecialKey KeyEnter) Down _ _) (Modo Single,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyRight) Down _ _) (Modo Single,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyLeft ) Down _ _) (Modo Single,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyDown ) Down _ _) (Modo Single,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Voltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolheModo (EventKey (SpecialKey KeyEnter) Down _ _) (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyRight) Down _ _) (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyLeft ) Down _ _) (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyDown ) Down _ _) (Modo Multi ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Voltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolheModo (EventKey (SpecialKey KeyEnter) Down _ _) (Modo Voltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyUp   ) Down _ _) (Modo Voltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolheModo (EventKey (SpecialKey KeyDown ) Down _ _) (Modo Voltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolheModo _ estado = estado
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
escolhePersonagem :: Event -> EstadoGloss -> EstadoGloss
-- SinglePlayer
escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Inicio1         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (Personagem1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Inicio2         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (Personagem2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Inicio3         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (Personagem3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltar,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyUp   ) Down _ _) (PersonagemVoltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (PersonagemVoltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Personagem2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (PersonagemVoltar ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Single,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer
escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (PersonagemM1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (InicioM1         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (PersonagemM1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (PersonagemM1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (PersonagemM1      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltarM,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (PersonagemM2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (InicioM2         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (PersonagemM2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (PersonagemM2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM3     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (PersonagemM2      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltarM,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (PersonagemM3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (InicioM3         ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyLeft ) Down _ _) (PersonagemM3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM2     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyRight) Down _ _) (PersonagemM3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM1     ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (PersonagemM3      ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemVoltarM,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem (EventKey (SpecialKey KeyUp   ) Down _ _) (PersonagemVoltarM ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyDown ) Down _ _) (PersonagemVoltarM ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (PersonagemM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
escolhePersonagem (EventKey (SpecialKey KeyEnter) Down _ _) (PersonagemVoltarM ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Modo Multi  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

escolhePersonagem _ estado = estado
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
perdeuJogo :: Event -> EstadoGloss -> EstadoGloss
-- SinglePlayer1
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Jogo1 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Inicio1     ,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu Jogo1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Menu1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu Menu1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Jogo1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Menu1 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])

-- SinglePlayer2
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Jogo2 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Inicio2     ,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu Jogo2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Menu2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu Menu2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Jogo2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Menu2 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])

-- SinglePlayer3
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Jogo3 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Inicio3     ,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu Jogo3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Menu3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu Menu3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu Jogo3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Menu3 ,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar,jogoRandom estadoInicial p1,jogoM,imagens,fundo,0,skins,[0,0])

-- MultiPlayer1
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM11,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM1       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM11,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM11 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM11,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM11 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM11,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM12,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM1       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM12,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM12 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM12,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM12 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM12,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramJ1,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM1           ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu EmpataramJ1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramS1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu EmpataramS1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramJ1 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramS1,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

-- MultiPlayer2
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM21,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM2       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM21,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM21 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM21,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM21 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM21,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM22,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM2       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM22,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM22 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM22,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM22 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM22,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramJ2,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM2           ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu EmpataramJ2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramS2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu EmpataramS2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramJ2 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramS2,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

-- MultiPlayer3
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM31,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM3       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM31,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM31 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM31,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM31 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM31,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogoM32,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM3       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu JogoM32,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu MenuM32 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu MenuM32,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu JogoM32 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu MenuM32,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar   ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramJ3,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (InicioM3           ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])
perdeuJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Perdeu EmpataramJ3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramS3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Perdeu EmpataramS3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Perdeu EmpataramJ3 ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
perdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu EmpataramS3,jogo,jogoM,imagens,fundo,tempo,skins,[p1,p2])   = (Opcoes Jogar       ,jogo,jogoRandomM estadoInicialM (p1+p2),imagens,fundo,0,skins,[0,0])

perdeuJogo _ estado = estado
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- SinglePlayer1
modoJogo :: Event -> EstadoGloss -> EstadoGloss
modoJogo (EventKey k Down _ _) (Inicio1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogo1, animaJogo jogo (Move Cima)    ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogo1, animaJogo jogo (Move Baixo)   ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogo1, animaJogo jogo (Move Esquerda),jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogo1, animaJogo jogo (Move Direita) ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa Retomar1, jogo,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (Char 'b')            -> (Bot1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
                  _        -> (Inicio1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogo1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo1,animaJogadorTronco jogo (Move Cima)    ,jogoM,imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogo1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo1,animaJogadorTronco jogo (Move Baixo)   ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogo1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo1,animaJogadorTronco jogo (Move Esquerda),jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo1,animaJogadorTronco jogo (Move Direita) ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (Pausa Retomar1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer2
modoJogo (EventKey k Down _ _) (Inicio2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogo2, animaJogo jogo (Move Cima)    ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogo2, animaJogo jogo (Move Baixo)   ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogo2, animaJogo jogo (Move Esquerda),jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogo2, animaJogo jogo (Move Direita) ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa Retomar2, jogo,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (Char 'b')            -> (Bot2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
                  _        -> (Inicio2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogo2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo2,animaJogadorTronco jogo (Move Cima)    ,jogoM,imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogo2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo2,animaJogadorTronco jogo (Move Baixo)   ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogo2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo2,animaJogadorTronco jogo (Move Esquerda),jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo2,animaJogadorTronco jogo (Move Direita) ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (Pausa Retomar2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- SinglePlayer3
modoJogo (EventKey k Down _ _) (Inicio3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogo3, animaJogo jogo (Move Cima)    ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogo3, animaJogo jogo (Move Baixo)   ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogo3, animaJogo jogo (Move Esquerda),jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogo3, animaJogo jogo (Move Direita) ,jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa Retomar3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao)
                  (Char 'b')            -> (Bot3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
                  _        -> (Inicio3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogo3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo3,animaJogadorTronco jogo (Move Cima)    ,jogoM,imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogo3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo3,animaJogadorTronco jogo (Move Baixo)   ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogo3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo3,animaJogadorTronco jogo (Move Esquerda),jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (ModoJogo3,animaJogadorTronco jogo (Move Direita) ,jogoM,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminou jogo) = (Pausa Retomar3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

--MultiPlayer 1
modoJogo (EventKey k Down _ _) (InicioM1,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Cima,Parado]    , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Baixo,Parado]   , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Esquerda,Parado], imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Direita,Parado] , imagens, fundo, tempo, skins, pontuacao)
                  ---
                  (Char 'w')            -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Cima]    , imagens, fundo, tempo, skins, pontuacao)
                  (Char 's')            -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Baixo]   , imagens, fundo, tempo, skins, pontuacao)
                  (Char 'a')            -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Esquerda], imagens, fundo, tempo, skins, pontuacao)
                  (Char 'd')            -> (ModoJogoM1, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Direita] , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa RetomarM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao)
                  _                     -> (InicioM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- move Jogador 1
modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM1 jogoM (Move Cima),imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM1 jogoM (Move Baixo),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM1 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM1 jogoM (Move Direita),imagens,fundo,tempo,skins,pontuacao)
-- move Jogador 2
modoJogo (EventKey (Char 'w')            Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM2 jogoM (Move Cima),imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (Char 's')            Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM2 jogoM (Move Baixo),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'a')            Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM2 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'd')            Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM1,jogo,animaJogadorTroncoM2 jogoM (Move Direita),imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogoM1, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (Pausa RetomarM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer 2
modoJogo (EventKey k Down _ _) (InicioM2,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Cima,Parado]    , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Baixo,Parado]   , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Esquerda,Parado], imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Direita,Parado] , imagens, fundo, tempo, skins, pontuacao)
                  ---
                  (Char 'w')            -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Cima]    , imagens, fundo, tempo, skins, pontuacao)
                  (Char 's')            -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Baixo]   , imagens, fundo, tempo, skins, pontuacao)
                  (Char 'a')            -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Esquerda], imagens, fundo, tempo, skins, pontuacao)
                  (Char 'd')            -> (ModoJogoM2, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Direita] , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa RetomarM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao)
                  _                     -> (InicioM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- move Jogador 1
modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM1 jogoM (Move Cima)    ,imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM1 jogoM (Move Baixo)   ,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM1 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM1 jogoM (Move Direita) ,imagens,fundo,tempo,skins,pontuacao)
-- move Jogador 2
modoJogo (EventKey (Char 'w')            Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM2 jogoM (Move Cima)    ,imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (Char 's')            Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM2 jogoM (Move Baixo)   ,imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'a')            Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM2 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'd')            Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM2,jogo,animaJogadorTroncoM2 jogoM (Move Direita) ,imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogoM2, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (Pausa RetomarM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer3
modoJogo (EventKey k Down _ _) (InicioM3,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,pontuacao) = 
        case k of (SpecialKey KeyUp   ) -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Cima,Parado]    , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyDown ) -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Baixo,Parado]   , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyLeft ) -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Esquerda,Parado], imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeyRight) -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Move Direita,Parado] , imagens, fundo, tempo, skins, pontuacao)
                  ---
                  (Char 'w')            -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Cima]    , imagens, fundo, tempo, skins, pontuacao)
                  (Char 's')            -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Baixo]   , imagens, fundo, tempo, skins, pontuacao)
                  (Char 'a')            -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Esquerda], imagens, fundo, tempo, skins, pontuacao)
                  (Char 'd')            -> (ModoJogoM3, jogo, animaJogoM (JogoM jogador1 jogador2 mapa) [Parado,Move Direita] , imagens, fundo, tempo, skins, pontuacao)
                  (SpecialKey KeySpace) -> (Pausa RetomarM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao)
                  _                     -> (InicioM3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- move Jogador 1
modoJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM1 jogoM (Move Cima),imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (SpecialKey KeyDown ) Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM1 jogoM (Move Baixo),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyLeft ) Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM1 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM1 jogoM (Move Direita),imagens,fundo,tempo,skins,pontuacao)
-- move Jogador 2
modoJogo (EventKey (Char 'w')            Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM2 jogoM (Move Cima),imagens,fundo,tempo,skins,pontuacao) 
modoJogo (EventKey (Char 's')            Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM2 jogoM (Move Baixo),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'a')            Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM2 jogoM (Move Esquerda),imagens,fundo,tempo,skins,pontuacao)
modoJogo (EventKey (Char 'd')            Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (ModoJogoM3,jogo,animaJogadorTroncoM2 jogoM (Move Direita),imagens,fundo,tempo,skins,pontuacao)

modoJogo (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogoM3, jogo, jogoM, imagens, fundo, tempo, skins, pontuacao) | not (jogoTerminouM jogoM) = (Pausa RetomarM3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

modoJogo _ estado = estado
---------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------
pausaJogo :: Event -> EstadoGloss -> EstadoGloss
-- SinglePlayer1
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Retomar1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogo1     ,animaJogo jogo Parado,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Retomar1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair1   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Retomar1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair1   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Sair1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Sair1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
-- SinglePlayer2
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Retomar2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogo2     ,animaJogo jogo Parado,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Retomar2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair2   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Retomar2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair2   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Sair2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Sair2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
-- SinglePlayer3
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Retomar3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogo3     ,animaJogo jogo Parado,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Retomar3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair3   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Retomar3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Sair3   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa Sair3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Sair3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa Retomar3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar  ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

-- MultiPlayer1
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa RetomarM1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogoM1     ,jogo,animaJogoM jogoM [Parado,Parado],imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa RetomarM1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM1   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa RetomarM1    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM1   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa SairM1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa SairM1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM1,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairM1       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
-- MultiPlayer2
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa RetomarM2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogoM2     ,jogo,animaJogoM jogoM [Parado,Parado],imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa RetomarM2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM2   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa RetomarM2    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM2   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa SairM2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa SairM2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM2,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairM2       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
-- MultiPlayer3
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa RetomarM3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (ModoJogoM3     ,jogo,animaJogoM jogoM [Parado,Parado],imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa RetomarM3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM3   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa RetomarM3    ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa SairM3   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyUp   ) Down _ _) (Pausa SairM3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa SairM3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Pausa RetomarM3,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)
pausaJogo (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairM3       ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao) = (Opcoes Jogar   ,jogo,jogoM,imagens,fundo,tempo,skins,pontuacao)

pausaJogo _ estado = estado
--------------------------------------------------------------------------------------------------------------------------


reageTempo :: Float -> EstadoGloss -> EstadoGloss
-- SinglePlayer1
reageTempo tempoDecorrido (ModoJogo1,jogo@(Jogo jogador mapa),jogoM,imagens,fundo,tempo,skins,[pontuacao,p]) 
        | jogoTerminou jogo          = (Perdeu Jogo1,jogo,jogoM,imagens,fundo,tempo,skins,[calculaPontuacao pontuacao jogador,p])
        | (mod (round tempo) 2) == 0 = (ModoJogo1,deslizaJogo seed (animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                  = (ModoJogo1,(animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao,p])
        where seed = round(tempoDecorrido) * pontuacao

reageTempo tempoDecorrido (Bot1,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p]) 
        | jogoTerminou jogo                                    = (Perdeu Jogo1,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p])
        | (mod (round (tempoDecorrido)*pontuacao*1000) 3) == 1 = (Bot1,deslizaJogo seed (bot jogo),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                                            = (Bot1,bot jogo,jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        where seed = round(tempoDecorrido) * pontuacao

-- SinglePlayer2
reageTempo tempoDecorrido (ModoJogo2,jogo@(Jogo jogador mapa),jogoM,imagens,fundo,tempo,skins,[pontuacao,p])
        | jogoTerminou jogo          = (Perdeu Jogo2,jogo,jogoM,imagens,fundo,tempo,skins,[calculaPontuacao pontuacao jogador,p])
        | (mod (round tempo) 2) == 0 = (ModoJogo2,deslizaJogo seed (animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                  = (ModoJogo2,(animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao,p])
        where seed = round(tempoDecorrido) * pontuacao

reageTempo tempoDecorrido (Bot2,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p]) 
        | jogoTerminou jogo                                    = (Perdeu Jogo2,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p])
        | (mod (round (tempoDecorrido)*pontuacao*1000) 3) == 1 = (Bot2,deslizaJogo seed (animaJogo (bot jogo) Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                                            = (Bot2,(animaJogo (bot jogo) Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        where seed = round(tempoDecorrido) * pontuacao

-- SinglePlayer3
reageTempo tempoDecorrido (ModoJogo3,jogo@(Jogo jogador mapa),jogoM,imagens,fundo,tempo,skins,[pontuacao,p])
        | jogoTerminou jogo          = (Perdeu Jogo3,jogo,jogoM,imagens,fundo,tempo,skins,[calculaPontuacao pontuacao jogador,p])
        | (mod (round tempo) 2) == 0 = (ModoJogo3,deslizaJogo seed (animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                  = (ModoJogo3,(animaJogo jogo Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao,p])
        where seed = round(tempoDecorrido) * pontuacao

reageTempo tempoDecorrido (Bot3,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p]) 
        | jogoTerminou jogo                                    = (Perdeu Jogo3,jogo,jogoM,imagens,fundo,tempo,skins,[pontuacao,p])
        | (mod (round (tempoDecorrido)*pontuacao*1000) 3) == 1 = (Bot3,deslizaJogo seed (animaJogo (bot jogo) Parado),jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pontuacao+1,p])
        | otherwise                                            = (Bot3,(animaJogo (bot jogo) Parado),jogoM,imagens,fundo,tempo,skins,[pontuacao+1,p])
        where seed = round(tempoDecorrido) * pontuacao

-- MultiPlayer1
reageTempo tempoDecorrido (ModoJogoM1,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,[p1,p2])
        | ambosPerderamM jogoM && pf1 == pf2                                                    = (Perdeu EmpataramJ1,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1<pf2) || (jogoTerminou (Jogo jogador1 mapa) && pf1 == pf2) = (Perdeu JogoM11,jogo,jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1>pf2) || (jogoTerminou (Jogo jogador2 mapa) && pf1 == pf2) = (Perdeu JogoM12,jogo,jogoM,imagens,fundo,tempo+tempoDecorrido,skins,[pf1,pf2])
        | (mod (round tempo) 2) == 0 = (ModoJogoM1,jogo,deslizaJogoM seed (animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1+1,p2+1])
        | otherwise                  = (ModoJogoM1,jogo,(animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1,p2])
        where seed = round(tempoDecorrido) * p1 * p2
              pf1 = calculaPontuacao p1 jogador1
              pf2 = calculaPontuacao p2 jogador2

-- MultiPlayer2
reageTempo tempoDecorrido (ModoJogoM2,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,[p1,p2])
        | ambosPerderamM jogoM && pf1 == pf2                                                    = (Perdeu EmpataramJ2,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1<pf2) || (jogoTerminou (Jogo jogador1 mapa) && pf1 == pf2) = (Perdeu JogoM21,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1>pf2) || (jogoTerminou (Jogo jogador2 mapa) && pf1 == pf2) = (Perdeu JogoM22,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (mod (round tempo) 2) == 0 = (ModoJogoM2,jogo,deslizaJogoM seed (animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1+1,p2+1])
        | otherwise                  = (ModoJogoM2,jogo,(animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1,p2])
        where seed = round(tempoDecorrido) * p1 * p2
              pf1 = calculaPontuacao p1 jogador1
              pf2 = calculaPontuacao p2 jogador2

-- MultiPlayer3
reageTempo tempoDecorrido (ModoJogoM3,jogo,jogoM@(JogoM jogador1 jogador2 mapa),imagens,fundo,tempo,skins,[p1,p2])
        | ambosPerderamM jogoM && pf1 == pf2                                                    = (Perdeu EmpataramJ3,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1<pf2) || (jogoTerminou (Jogo jogador1 mapa) && pf1 == pf2) = (Perdeu JogoM31,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (jogoTerminouM jogoM && pf1>pf2) || (jogoTerminou (Jogo jogador2 mapa) && pf1 == pf2) = (Perdeu JogoM32,jogo,jogoM,imagens,fundo,tempo,skins,[pf1,pf2])
        | (mod (round tempo) 2) == 0 = (ModoJogoM3,jogo,deslizaJogoM seed (animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1+1,p2+1])
        | otherwise                  = (ModoJogoM3,jogo,(animaJogoM jogoM [Parado,Parado]),imagens,fundo,tempo+tempoDecorrido,skins,[p1,p2])
        where seed = round(tempoDecorrido) * p1 * p2
              pf1 = calculaPontuacao p1 jogador1
              pf2 = calculaPontuacao p2 jogador2

reageTempo _ s = s

{-| A função 'calculaPontuacao' determina quantos pontos o Jogador realizou no final do Jogo, ou seja,
verifica quantos terrenos este avançou. 
De modo, que como o jogador começa na posição 10, a posição inicial do jogador no final do jogo
, com as linhas adicionadas desdo início do jogo com a 'deslizaJogo', será 10 + número de linhas extra.

Para saber onde o jogador perdeu, apenas subtraimos a posição inicial com a posição final do Jogador (10+nLinhas - yfinal).

A função poderia ser definida da seguinte forma:

@
calculaPontuacao :: Int -> Jogador -> Pontuacao
calculaPontuacao nLinhasExtra (Jogador (_,yf)) = 10 + nLinhasExtra - yf@

== Exemplos de utilização:

>>> calculaPontuacao 4 (Jogador (3,6))
8

>>> calculaPontuacao 17 (Jogador (3,0))
27
-}

calculaPontuacao :: Int -> Jogador -> Pontuacao
calculaPontuacao nLinhasExtra (Jogador (_,yf)) = 10 + nLinhasExtra - yf