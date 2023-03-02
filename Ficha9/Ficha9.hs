module Ficha9 where

import System.Random

-- Exercício 1:

-- a)

bingo :: IO ()
bingo = do nums <- sorteio []
           print $ reverse nums

sorteio :: [Int] -> IO [Int]
sorteio l | length l == 9 = return l
          | otherwise = do n <- randomRIO (1,90)
                           getChar
                           print n
                           if elem n l then sorteio l else sorteio (n:l)

-- b)

mastermind :: IO ()
mastermind = sequence (replicate 4 $ uniformRM (0,9) globalStdGen) >>= mastermindAux

mastermindAux = undefined
