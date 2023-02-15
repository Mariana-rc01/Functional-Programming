module Spec where

import Test.HUnit

import qualified Fact_spec as Fact
import qualified Aula4_Spec as Aula4

main = runTestTTAndExit $ test [Fact.testes,Aula4.testes]

-- No terminal, na diretoria da Aula4: runhaskell -i="src" -i="tests" tests/Spec.hs ou se quiser entrar no ghci: ghci -i="src" -i="tests" tests/Aula4_Spec.hs
