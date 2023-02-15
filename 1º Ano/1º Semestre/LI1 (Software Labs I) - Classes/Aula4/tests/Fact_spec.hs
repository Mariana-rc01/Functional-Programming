-- Passo 3:
module Fact_spec where

import Test.HUnit

import Fact

testes = TestLabel "Fact" $ test ["fact 0" ~: 1 ~=? fact 0
                                , "fact 5" ~: 120 ~=? fact 5]

-- Passo 4: ghci -i="src" -i="tests" tests/Aula4_Spec.hs

-- Passo 5: git add.
-- git commit -m 'Adicionado exemplo ficha5 1.5'
-- git push origin main ou git push
