module Ficha6 where

-- Exercício 1:

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t :: BTree Int
t = Node 5 (Node 3 Empty 
                   (Node 2 Empty Empty)) 
           (Node 4 Empty Empty)

-- a) Confirmada na aula

altura :: BTree a -> Int
altura Empty        = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

-- b) Confirmada na aula

contaNodos :: BTree a -> Int
contaNodos Empty        = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

-- c) Confirmada na aula

folhas :: BTree a -> Int
folhas Empty                = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d)         = folhas e + folhas d

-- d) Confirmada na aula

prune :: Int -> BTree a -> BTree a
prune _ Empty        = Empty
prune 0 _            = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

-- e) Confirmada na aula

path :: [Bool] -> BTree a -> [a]
path _ Empty            = []
path [] (Node r _ _)    = [r]
path (h:t) (Node r e d) = r : (if h then path t d else path t e)

-- f)

mirror :: BTree a -> BTree a
mirror Empty        = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

-- g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _                             = Empty

-- h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty                 = (Empty,Empty,Empty)
unzipBT (Node (r1,r2,r3) e d) = (Node r1 e1 d1,Node r2 e2 d2,Node r3 e3 d3)
                     where (e1,e2,e3) = unzipBT e
                           (d1,d2,d3) = unzipBT d

-- Exercício 2:

tP :: BTree Int
tP = Node 10 
             (Node 2 (Node 1 Empty Empty) 
                     (Node 7 Empty Empty)) 
            (Node 20 (Node 15 Empty Empty) 
                      Empty)

-- a) Confirmada na aula

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e _)     = minimo e

-- b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node r e d)     = Node r (semMinimo e) d

-- c)

minSim :: Ord a => BTree a -> (a,BTree a)
minSim (Node r Empty _) = (r,Empty)
minSim (Node r e d)     = (a,Node r b d)
                        where (a,b) = minSim e

-- d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d) | x < r = Node r (remove x e) d
                      | x > r = Node r e (remove x d)
                      | otherwise = case d of Empty -> e
                                              _     -> let (g,h) = minSim d in Node g e h

-- Exercício 3:

type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int

type Nome = String

data Regime = ORD
            | TE 
            | MEL 
            deriving (Show,Eq)

data Classificacao = Aprov Int 
                   | Rep 
                   | Faltou 
                   deriving (Show,Eq)

type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


-- Exemplo de turma:
turma :: Turma
turma = (Node (15,"Luís",ORD,Aprov 14) 
                   (Node (12,"Joana",MEL,Faltou)
                                    (Node (7,"Diogo",TE,Rep) Empty Empty) 
                                    (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) 
                   (Node (20,"Elisa",TE,Aprov 10) Empty 
                                    (Node (25,"Mariana",ORD,Aprov 20) 
                                            (Node (23,"Hugo",ORD,Aprov 17) Empty Empty) 
                                            (Node (28,"Vasco",MEL,Rep) Empty Empty))))

-- a) Confirmada na aula

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) = n == num || inscNum n (if n > num then d else e)

-- ou

inscNum' :: Numero -> Turma -> Bool
inscNum' _ Empty = False
inscNum' n (Node (num,_,_,_) e d) | n == num = True
                                  | n > num  = inscNum' n d
                                  | n < num  = inscNum' n e

-- b) Confirmada na aula

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) = n == nome || inscNome n e || inscNome n d

-- c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,regime,_) e d) = if regime == TE then trabEst e++[(num,nome)]++trabEst d
                                         else trabEst e ++ trabEst d

-- d) Confirmada na aula

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,classi) e d) | n == num  = Just classi
                                   | n > num   = nota n d
                                   | n < num   = nota n e
                                   | otherwise = Nothing

-- e)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumFaltas turma / nAlunos turma * 100
                 where nAlunos = fromIntegral . contaNodos
                       sumFaltas :: Turma -> Float
                       sumFaltas Empty = 0
                       sumFaltas (Node (_,_,_,classi) e d) = if classi == Faltou then 1 + sumFaltas e + sumFaltas d
                                                             else sumFaltas e + sumFaltas d

-- f)

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = fst (sumAprov turma) / snd (sumAprov turma)
                 where sumAprov :: Turma -> (Float,Float)
                       sumAprov Empty = (0,0)
                       sumAprov (Node (_,_,_,Aprov a) e d) = adicionaPares (fromIntegral a,1) (adicionaPares (sumAprov e) (sumAprov d))
                       sumAprov (Node _ e d) = adicionaPares (sumAprov e) (sumAprov d)
                       adicionaPares (a,b) (c,d) = (a+c,b+d)

-- g)

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = fst (sumAprovAv turma) / snd (sumAprovAv turma)

sumAprovAv :: Turma -> (Float,Float)
sumAprovAv Empty = (0,0)
sumAprovAv (Node (_,_,_,classi) e d) = case classi of Aprov a -> (apr+1,repr)
                                                      Rep     -> (apr,repr+1)
                                                      _       -> (apr,repr)
          where (apr,repr) = adicionaPares (sumAprovAv e) (sumAprovAv d)
                adicionaPares (a,b) (c,d) = (a+c,b+d)