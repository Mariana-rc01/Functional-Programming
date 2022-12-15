-- Passo 1:
module Fact where
import Test.HUnit
-- | A função 'fact' dá-me o fatorial.
--
-- == Exemplos de utilização:
-- >>> fact 0
-- 1

fact :: Int -> Int
fact 0 = 1
fact n = n*fact(n-1)

-- Passo 2: documentar em haddock -h -o doc/html src/Aula4.hs
