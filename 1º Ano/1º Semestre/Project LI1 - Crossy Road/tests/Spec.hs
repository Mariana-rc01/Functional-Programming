module Main where

import Tarefa1_2022li1g007_Spec
import Tarefa2_2022li1g007_Spec
import Tarefa3_2022li1g007_Spec
import Tarefa4_2022li1g007_Spec
import Tarefa5_2022li1g007_Spec
import MultiPlayerTarefa3_2022li1g007_Spec
import MultiPlayerTarefa4e5_2022li1g007_Spec
import Bot_2022li1g007_Spec
import Test.HUnit

runTestsT1 = runTestTT testsT1

runTestsT2 = runTestTT testsT2

runTestsT3 = runTestTT testsT3

runTestsT4 = runTestTT testsT4

runTestsT5 = runTestTT testsT5

runTestsTM3 = runTestTT testsTM3

runTestsTM4 = runTestTT testsTM4

runTestsTM5 = runTestTT testsTM5

runTestsTB = runTestTT testsTB

main = runTestTTAndExit $ TestList [testsT1, testsT2, testsT3, testsT4, testsT5, testsTM3, testsTM4, testsTM5, testsTB]
