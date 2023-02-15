module Aula4_Spec where

import Test.HUnit
import Aula4

-- Ex1:
-- Exemplos para de listas de strings:

l1 = ["Joaquim", "Francisco", "Alves", "Martins"]
l2 = ["Mariana", "Ana", "Alice"]
l3 = []

-- Testes do ex1:

teste1 =  "Testa lista l1" ~: "J.Martins" ~=? lNames l1
teste2 =  "Testa lista l2" ~: "M.Alice" ~=? lNames l2
teste3 =  "Testa lista l3" ~: "" ~=? lNames l3

--testes_lNames = test [teste1, teste2, teste3]

-- Ex2:
-- Exemplos de listas de pessoas:

pp1 = [Pos "Alberto Junqueira" (1,1), Pos "Alice Xavier" (0,2), Pos "Zeferino Campos" (1,2)]
pp2 = [(Pos "Maria" (2,3)),(Pos "José" (1,4)),(Pos "Hugo" (3,5)),(Pos "Mariana" (4,5))]
pp3 = [(Pos "Maria" (2,-3)),(Pos "José" (1,-4)),(Pos "Hugo" (3,-5)),(Pos "Mariana" (4,6))]

-- Testes do ex2:
-- Função auxiliar:

teste4 =  "Testa ordenalista: pp1" ~: 2 ~=? (ordenalista pp1)
teste5 =  "Testa ordenalista pp2" ~: 5 ~=? (ordenalista pp2)
teste6 =  "Testa ordenalista pp3" ~: 6 ~=? (ordenalista pp3)

--testes_ordenalista = test [teste4, teste5, teste6]

-- Função pessoasNorte:

teste7 =  "Testa pessoasNorte pp1" ~: ["Alice Xavier", "Zeferino Campos"] ~=? (pessoasNorte pp1)
teste8 =  "Testa pessoasNorte pp2" ~: ["Hugo","Mariana"] ~=? (pessoasNorte pp2)
teste9 =  "Testa pessoasNorte pp3" ~: ["Mariana"] ~=? (pessoasNorte pp3)

--testes_pessoasNorte = test [teste7, teste8, teste9]

-- Ex3:

-- Exemplos de listas:

lista1 = "ola"
lista2 = [1,2,3,4]

-- Testes do ex3:

teste10 =  "Testa subs l1" ~: "Ola" ~=? (subs lista1 0 'O')
teste11 =  "Testa subs l2" ~: [1,5,3,4] ~=? (subs lista2 2 5)
teste12 =  "Testa subs l2" ~: [1,2,3,4] ~=? (subs lista2 5 6)

--testes_subs = test [teste10, teste11, teste12]

testes = TestLabel "Aula4_spec" $ test [teste1, teste2, teste3,
                                        teste4, teste5, teste6, 
                                        teste7, teste8, teste9, 
                                        teste10, teste11, teste12]