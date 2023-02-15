{- |
Module      : Main
Description : Main do projeto
Copyright   : Mariana Rocha Cristino <a90817@alunos.uminho.pt>
              Ana Carolina Penha Cerqueira <a104188@alunos.uminho.pt>

Módulo para a realização do projeto de LI1 em 2022/23.
-}

module Main where

import LI12223
import Tarefa1_2022li1g007
import Tarefa2_2022li1g007
import Tarefa3_2022li1g007
import Tarefa4_2022li1g007
import Tarefa5_2022li1g007
import MultiPlayerTarefa3_2022li1g007
import MultiPlayerTarefa4e5_2022li1g007
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Desenha_2022li1g007
import AlteraJogo_2022li1g007
import Bot_2022li1g007

main :: IO ()
main = do
    relvaO      <- loadBMP "Imagens/relva/grass.bmp"
    rioO        <- loadBMP "Imagens/relva/agua.bmp"
    estradaO    <- loadBMP "Imagens/relva/estrada.bmp"
    carroO      <- loadBMP "Imagens/relva/carro.bmp"
    carroOR     <- loadBMP "Imagens/relva/carroR.bmp"
    camiaoO     <- loadBMP "Imagens/relva/camiao.bmp"
    camiaoOR    <- loadBMP "Imagens/relva/camiaoR.bmp"
    autocarroO  <- loadBMP "Imagens/relva/autocarro.bmp"
    autocarroOR <- loadBMP "Imagens/relva/autocarroR.bmp"
    troncoO     <- loadBMP "Imagens/relva/tronco.bmp"
    arvoreO     <- loadBMP "Imagens/relva/arvore.bmp"
    galinha     <- loadBMP "Imagens/relva/jogador1.bmp"
    pato        <- loadBMP "Imagens/relva/pato.bmp"

    relvaN      <- loadBMP "Imagens/natal/snow.bmp"
    rioN        <- loadBMP "Imagens/natal/ice.bmp"
    estradaN    <- loadBMP "Imagens/natal/roadIce.bmp"
    carroN      <- loadBMP "Imagens/natal/treno.bmp"
    carroNR     <- loadBMP "Imagens/natal/trenoR.bmp"
    camiaoN     <- loadBMP "Imagens/natal/trenoMedio.bmp"
    camiaoNR    <- loadBMP "Imagens/natal/trenoMedioR.bmp"
    autocarroN  <- loadBMP "Imagens/natal/trenoGrande.bmp"
    autocarroNR <- loadBMP "Imagens/natal/trenoGrandeR.bmp"
    troncoN     <- loadBMP "Imagens/relva/tronco.bmp"
    arvoreN     <- loadBMP "Imagens/natal/arvoreNatal.bmp"
    bolacha     <- loadBMP "Imagens/natal/bolacha.bmp"
    boneco      <- loadBMP "Imagens/natal/boneco_de_neve.bmp"

    relvaL      <- loadBMP "Imagens/lava/estrada.bmp"
    rioL        <- loadBMP "Imagens/lava/rio.bmp"
    estradaL    <- loadBMP "Imagens/lava/relva.bmp"
    carroL      <- loadBMP "Imagens/lava/bola1R.bmp"
    carroLR     <- loadBMP "Imagens/lava/bola1.bmp"
    camiaoL     <- loadBMP "Imagens/lava/bola2.bmp"
    camiaoLR    <- loadBMP "Imagens/lava/bola2R.bmp"
    autocarroL  <- loadBMP "Imagens/lava/bola3.bmp"
    autocarroLR <- loadBMP "Imagens/lava/bola3R.bmp"
    troncoL     <- loadBMP "Imagens/lava/troncoLava.bmp"
    arvoreL     <- loadBMP "Imagens/lava/arvoreLava.bmp"
    dragao1     <- loadBMP "Imagens/lava/dragao1.bmp"
    dragao2     <- loadBMP "Imagens/lava/dragao2.bmp"


    let imagensOriginal = [("Arvore" ,(scale 0.25 0.25 $ arvoreO ,(0,0))),
          ("Carro"     ,(scale 0.25 0.25 $ carroOR    ,(0,0))),
          ("CarroR"    ,(scale 0.25 0.25 $ carroO     ,(0,0))),
          ("Camiao"    ,(scale 0.25 0.25 $ camiaoOR   ,(-50,0))),
          ("CamiaoR"   ,(scale 0.25 0.25 $ camiaoO    ,(-50,0))),
          ("Autocarro" ,(scale 0.25 0.25 $ autocarroOR,(0,0))),
          ("AutocarroR",(scale 0.25 0.25 $ autocarroO ,(0,0))),
          ("Tronco"    ,(scale 0.25 0.25 $ troncoO    ,(0,0))),
          ("Nenhum"    ,(      Blank                  ,(0,0))),
          ("Relva"     ,(scale 0.25 0.25 $ relvaO     ,(0,0))),
          ("Estrada"   ,(scale 0.25 0.25 $ estradaO   ,(0,0))),
          ( "Rio"      ,(scale 0.25 0.25 $ rioO       ,(0,0)))]

    let imagensNatal = [("Arvore" ,(scale 0.25 0.25 $ arvoreN ,(0,0))),
          ("Carro"  ,(scale 0.25 0.25 $ carroNR ,(0,0))),
          ("CarroR" ,(scale 0.25 0.25 $ carroN  ,(0,0))),
          ("Camiao"    ,(scale 0.25 0.25 $ camiaoNR   ,(-50,0))),
          ("CamiaoR"   ,(scale 0.25 0.25 $ camiaoN    ,(-50,0))),
          ("Autocarro" ,(scale 0.25 0.25 $ autocarroNR,(0,0))),
          ("AutocarroR",(scale 0.25 0.25 $ autocarroN ,(0,0))),
          ("Tronco" ,(scale 0.25 0.25 $ troncoN ,(0,0))),
          ("Nenhum" ,(      Blank               ,(0,0))),
          ("Relva"  ,(scale 0.25 0.25 $ relvaN  ,(0,0))),
          ("Estrada",(scale 0.65 0.65 $ estradaN,(0,0))),
          ( "Rio"   ,(scale 0.25 0.25 $ rioN    ,(0,0)))]

    let imagensLava  = [("Arvore" ,(scale 0.12 0.12 $ arvoreL ,(0,0))),
          ("Carro"  ,(scale 0.25 0.25 $ carroL  ,(0,0))),
          ("CarroR" ,(scale 0.25 0.25 $ carroLR ,(0,0))),
          ("Camiao"    ,(scale 0.25 0.25 $ camiaoLR   ,(-50,0))),
          ("CamiaoR"   ,(scale 0.25 0.25 $ camiaoL    ,(-50,0))),
          ("Autocarro" ,(scale 0.25 0.25 $ autocarroLR,(0,0))),
          ("AutocarroR",(scale 0.25 0.25 $ autocarroL ,(0,0))),
          ("Tronco" ,(scale 0.25 0.25 $ troncoL ,(0,0))),
          ("Nenhum" ,(      Blank               ,(0,0))),
          ("Relva"  ,(scale 0.25 0.25 $ relvaL  ,(0,0))),
          ("Estrada",(scale 0.25 0.25 $ estradaL,(0,0))),
          ( "Rio"   ,(scale 0.25 0.25 $ rioL    ,(0,0)))]

    let imagens = [imagensOriginal, imagensNatal, imagensLava]

    menuJogar      <- loadBMP "Imagens/Fundo/Menu/menuJogar.bmp"
    menuInstrucoes <- loadBMP "Imagens/Fundo/Menu/menuInstrucoes.bmp"
    menuSair       <- loadBMP "Imagens/Fundo/Menu/menuSair.bmp"

    menuModoMulti  <- loadBMP "Imagens/Fundo/ModoJogo/menumodoMulti.bmp"
    menuModoSingle <- loadBMP "Imagens/Fundo/ModoJogo/menumodoSingle.bmp"
    menuModoVoltar <- loadBMP "Imagens/Fundo/ModoJogo/menumodoVoltar.bmp"

    modoInstrucoes1 <- loadBMP "Imagens/Fundo/Instrucoes/menuregras1.bmp"
    modoInstrucoesC <- loadBMP "Imagens/Fundo/Instrucoes/menuregras2.bmp"
    modoInstrucoesS <- loadBMP "Imagens/Fundo/Instrucoes/menuregrassair.bmp"

    pausaRetomar <- loadBMP "Imagens/Fundo/Pausa/pausaRetomar.bmp"
    pausaSair    <- loadBMP "Imagens/Fundo/Pausa/pausaSair.bmp"

    personagem1      <- loadBMP "Imagens/Fundo/Personagens/Single/personagem1.bmp"
    personagem2      <- loadBMP "Imagens/Fundo/Personagens/Single/personagem2.bmp"
    personagem3      <- loadBMP "Imagens/Fundo/Personagens/Single/personagem3.bmp"
    personagemVoltar <- loadBMP "Imagens/Fundo/Personagens/Single/personagemVoltar.bmp"

    personagem1M      <- loadBMP "Imagens/Fundo/Personagens/Multi/personagem1M.bmp"
    personagem2M      <- loadBMP "Imagens/Fundo/Personagens/Multi/personagem2M.bmp"
    personagem3M      <- loadBMP "Imagens/Fundo/Personagens/Multi/personagem3M.bmp"
    personagemVoltarM <- loadBMP "Imagens/Fundo/Personagens/Multi/personagemVoltarM.bmp"

    modoJogo1  <- loadBMP "Imagens/Fundo/Jogo/jogo1.bmp"
    modoJogo2  <- loadBMP "Imagens/Fundo/Jogo/jogo2.bmp"
    modoJogo3  <- loadBMP "Imagens/Fundo/Jogo/jogo3.bmp"

    perdeuSair   <- loadBMP "Imagens/Fundo/Perdeu/Single/perdeuSair.bmp"
    perdeuTentar <- loadBMP "Imagens/Fundo/Perdeu/Single/perdeuTentar.bmp"

    perdeuTentarM11       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuGalinha.bmp"
    perdeuSairM11         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairGalinha.bmp"
    perdeuTentarM12       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuPato.bmp"
    perdeuSairM12         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairPato.bmp"
    perdeuEmpataramJogar1 <- loadBMP "Imagens/Fundo/Perdeu/Multi/Empataram1.bmp"
    perdeuEmpataramSair1  <- loadBMP "Imagens/Fundo/Perdeu/Multi/EmpataramSair1.bmp"

    perdeuTentarM21       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuBolacha.bmp"
    perdeuSairM21         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairBolacha.bmp"
    perdeuTentarM22       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuNeve.bmp"
    perdeuSairM22         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairNeve.bmp"
    perdeuEmpataramJogar2 <- loadBMP "Imagens/Fundo/Perdeu/Multi/Empataram2.bmp"
    perdeuEmpataramSair2  <- loadBMP "Imagens/Fundo/Perdeu/Multi/EmpataramSair2.bmp"

    perdeuTentarM31       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuBranco.bmp"
    perdeuSairM31         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairBranco.bmp"
    perdeuTentarM32       <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuPreto.bmp"
    perdeuSairM32         <- loadBMP "Imagens/Fundo/Perdeu/Multi/PerdeuSairPreto.bmp"
    perdeuEmpataramJogar3 <- loadBMP "Imagens/Fundo/Perdeu/Multi/Empataram3.bmp"
    perdeuEmpataramSair3  <- loadBMP "Imagens/Fundo/Perdeu/Multi/EmpataramSair3.bmp"


    n1 <- loadBMP "Imagens/Numeros/1.bmp"
    n2 <- loadBMP "Imagens/Numeros/2.bmp"
    n3 <- loadBMP "Imagens/Numeros/3.bmp"
    n4 <- loadBMP "Imagens/Numeros/4.bmp"
    n5 <- loadBMP "Imagens/Numeros/5.bmp"
    n6 <- loadBMP "Imagens/Numeros/6.bmp"
    n7 <- loadBMP "Imagens/Numeros/7.bmp"
    n8 <- loadBMP "Imagens/Numeros/8.bmp"
    n9 <- loadBMP "Imagens/Numeros/9.bmp"
    n0 <- loadBMP "Imagens/Numeros/0.bmp"


    let imagensFundo = [("menuJogar"     ,scale 0.4 0.35 $ menuJogar),
                        ("menuInstrucoes",scale 0.4 0.35 $ menuInstrucoes),
                        ("menuSair"      ,scale 0.4 0.35 $ menuSair),

                        ("modoSingle",scale 0.4 0.35 $ menuModoSingle),
                        ("modoMulti" ,scale 0.4 0.35 $ menuModoMulti),
                        ("modoVoltar",scale 0.4 0.35 $ menuModoVoltar),

                        ("modoInstrucoes1",scale 0.4 0.35 $ modoInstrucoes1),
                        ("modoInstrucoesC",scale 0.4 0.35 $ modoInstrucoesC),
                        ("modoInstrucoesS",scale 0.4 0.35 $ modoInstrucoesS),

                        ("pausaRetomar",scale 0.4 0.35 $ pausaRetomar),
                        ("pausaSair"   ,scale 0.4 0.35 $ pausaSair),
                        ("perdeuSair"  ,scale 0.4 0.35 $ perdeuSair),
                        ("perdeuTentar",scale 0.4 0.35 $ perdeuTentar),

                        ("personagem1"     ,scale 0.4 0.35 $ personagem1),
                        ("personagem2"     ,scale 0.4 0.35 $ personagem2),
                        ("personagem3"     ,scale 0.4 0.35 $ personagem3),
                        ("personagemVoltar",scale 0.4 0.35 $ personagemVoltar),

                        ("personagem1M"     ,scale 0.4 0.35 $ personagem1M),
                        ("personagem2M"     ,scale 0.4 0.35 $ personagem2M),
                        ("personagem3M"     ,scale 0.4 0.35 $ personagem3M),
                        ("personagemVoltarM",scale 0.4 0.35 $ personagemVoltarM),

                        ("perdeuSairM11"  ,scale 0.4 0.35 $ perdeuSairM11),
                        ("perdeuTentarM11",scale 0.4 0.35 $ perdeuTentarM11),
                        ("perdeuSairM12"  ,scale 0.4 0.35 $ perdeuSairM12),
                        ("perdeuTentarM12",scale 0.4 0.35 $ perdeuTentarM12),
                        ("perdeuEmpataramJ1",scale 0.4 0.35 $ perdeuEmpataramJogar1),
                        ("perdeuEmpataramS1",scale 0.4 0.35 $ perdeuEmpataramSair1),


                        ("perdeuSairM21"  ,scale 0.4 0.35 $ perdeuSairM21),
                        ("perdeuTentarM21",scale 0.4 0.35 $ perdeuTentarM21),
                        ("perdeuSairM22"  ,scale 0.4 0.35 $ perdeuSairM22),
                        ("perdeuTentarM22",scale 0.4 0.35 $ perdeuTentarM22),
                        ("perdeuEmpataramJ2",scale 0.4 0.35 $ perdeuEmpataramJogar2),
                        ("perdeuEmpataramS2",scale 0.4 0.35 $ perdeuEmpataramSair2),

                        ("perdeuSairM31"  ,scale 0.4 0.35 $ perdeuSairM31),
                        ("perdeuTentarM31",scale 0.4 0.35 $ perdeuTentarM31),
                        ("perdeuSairM32"  ,scale 0.4 0.35 $ perdeuSairM32),
                        ("perdeuTentarM32",scale 0.4 0.35 $ perdeuTentarM32),
                        ("perdeuEmpataramJ3",scale 0.4 0.35 $ perdeuEmpataramJogar3),
                        ("perdeuEmpataramS3",scale 0.4 0.35 $ perdeuEmpataramSair3),

                        ("modoJogo1" ,modoJogo1),
                        ("modoJogo2" ,modoJogo2),
                        ("modoJogo3" ,modoJogo3)
                        ]

    let numeros = [("1",scale 0.4 0.35 $ n1),
                   ("2",scale 0.4 0.35 $ n2),
                   ("3",scale 0.4 0.35 $ n3),
                   ("4",scale 0.4 0.35 $ n4),
                   ("5",scale 0.4 0.35 $ n5),
                   ("6",scale 0.4 0.35 $ n6),
                   ("7",scale 0.4 0.35 $ n7),
                   ("8",scale 0.4 0.35 $ n8),
                   ("9",scale 0.4 0.35 $ n9),
                   ("0",scale 0.4 0.35 $ n0)
                  ]

    play FullScreen
         orange
         fr
          (estadoGlossInicial 
         imagens
         (imagensFundo ++ numeros)
         [(Jogador (0,0),"Galinha", scale 0.25  0.25 $ galinha ),
          (Jogador (0,0),"Bolacha", scale 0.2   0.25 $ bolacha ),
          (Jogador (0,0),"Dragao1", scale 0.3   0.3  $ dragao1),
          (Jogador (0,0),"Pato"   , scale 0.125 0.15 $ pato),
          (Jogador (0,0),"Boneco" , scale 0.5   0.5  $ boneco),
          (Jogador (0,0),"Dragao2", scale 0.08  0.08 $ dragao2)
         ])
         desenhaEstado
         reageEvento
         reageTempo