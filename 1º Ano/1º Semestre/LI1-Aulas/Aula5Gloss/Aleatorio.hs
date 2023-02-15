module Aleatorio where

import System.Random
import Data.List.Split
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{-
duplica :: Num a => a -> a
duplica x = x*x

aleatorio :: IO Int
aleatorio = do
    x <- randomRIO (0,100)
    y <- randomRIO (0,100)

    let c = duplica x
 
    putStrLn ("x: " ++ show x ++ "//y: " ++ show y)
    putStrLn ("c: " ++ show c)
    putStrLn "Terminou..."

    return (x*y)

-- se fizermos p = aleatorio, gera sempre x e y diferentes
-- se fizermos d <- aleatorio, "guarda" o produto 
-- expose modules meter tarefa5
-}

data Terreno = Rio | Estrada | Relva deriving Show

toTerreno :: Char -> Terreno
toTerreno '-' = Estrada
toTerreno '~' = Rio
toTerreno '^' = Relva

ler :: IO [Terreno]
ler = do 
    estado <- readFile "exemplo.txt"
    let linhas = lines estado

    let [x,y] = splitOn " " $ head linhas
    let i = read x :: Int
    let j = read y :: Int
    let coordenadas = (i,j)
    putStrLn ("Jogador: " ++ show i ++ "," ++ show j)
    let o = map toTerreno $ map head $ tail linhas
    return o

{-
separarVirgula :: String -> [String]
separarVirgula linha = splitOn "," linha

gastos :: IO Int
gastos = do
    dados <- readFile "contas.csv"

    let valores = map (read::Int) $ map separarVirgula $ tail $ lines dados

    return $ sum valores
-}
{-
-- Como organizar código:
import Data.Maybe

data Terreno = Rio | Relva | Estrada
type Images = [(Terreno, Picture)]

-- fromJust $ lookup "Amor" [("Amor","às vezes é bom"),("Felicidade","é bom")] palavra e dicionário, retira o Just, só
-- podemos fazer isso se existir mesmo e isso acontece porque somos nós que definimos o código

main = do
	rio <- loadBMP "rio.bmp"
	estrada <- loadBMP "estrada.bmp"
	relva <- loadBMP "relva.bmp"
	images [(Dark,imagesDark),(Light,imagesLight)] -- caso quisessemos mapa
	let imagens = [(Rio,rio),(Estrada,estrada),(Relva,relva)]
	let rio = fromJust $ lookup Rio imagens-}