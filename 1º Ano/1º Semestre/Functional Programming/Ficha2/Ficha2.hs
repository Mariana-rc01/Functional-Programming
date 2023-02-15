module Ficha2 where
import Data.Char

-- Exercício 2:

-- a) Confirmada na aula

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h : dobros t)


-- b) Confirmada na aula

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre a (h:t) = if a==h then 1 + (numOcorre a t) else numOcorre a t

-- c) Confirmada na aula

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h>0 then positivos t else False

-- d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h>0 then (h:soPos t) else soPos t

-- e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h<0 = h + somaNeg t
              | otherwise = somaNeg t

-- f) Confirmada na aula

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if (length (h:t)) < 3 || (length (h:t)) == 3 then (h:t) else tresUlt t

-- g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):w) = (y : segundos w)

-- h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,y):t) = if a == x then True else nosPrimeiros a t

-- i) Confirmada na aula

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+sX,y+sY,z+sZ) where (sX,sY,sZ) = sumTriplos t

-- Exercício 3:

-- a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if ((ord h)>47 && (ord h)<58) then (h:soDigitos t) else soDigitos t

-- b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | (ord h)>96 && (ord h)<123 = 1 + minusculas t
                 | otherwise = minusculas t

-- c)

nums :: String -> [Int]
nums [] = []
nums (h:t) = if elem h ['0'..'9'] then (ord h - ord '0') : nums t else nums t

-- Exercício 4:

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) Confirmada na aula

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n (h:t) = if n == snd h then 1 + (conta n t) else (conta n t)

-- b) Confirmada na aula

grau :: Polinomio -> Int
grau [] = 0
grau (h:t) | snd h > m = snd h
           | otherwise = m
           where m = grau t

-- c) Confirmada na aula

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g (h:t) = if g == snd h then (h:selgrau g t) else selgrau g t

-- d) Confirmada na aula

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = if y > 0 then (x*fromIntegral y,y-1):deriv t else deriv t

-- e) Confirmada na aula

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula a ((x,y):t) = x*a^y + (calcula a t)

-- f)

simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) = if fst h == 0 then simp t else (h:simp t)

-- g)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult n (h:t) = (fst n * fst h, snd n + snd h):mult n t

-- h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x,y)] = [(x,y)]
normaliza ((x,y):(a,b):t) | y == b = normaliza ((x+a,y):t)
                          | conta y t == 0 = (x,y):(normaliza ((a,b):t))
                          | otherwise = normaliza ((a,b):t ++ [(x,y)])

-- i)

soma :: Polinomio -> Polinomio -> Polinomio
soma l1 l2 = normaliza (l1 ++ l2)

-- j)

produto :: Polinomio -> Polinomio -> Polinomio
produto ((a,b):t) l = soma (mult (a,b) l) (produto t l)

-- k)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):t) = ordena (altos t) ++ [(a,b)] ++ ordena (baixos t)
                 where altos [] = []
                       altos ((ax,bx):ts) = if bx > b || (bx == b && ax >= a) then (ax,bx):altos ts else altos ts
                       baixos [] = []
                       baixos ((ax,bx):ts) = if bx < b || (bx == b && ax < a) then (ax,bx):baixos ts else baixos ts


-- l)

equiv :: Polinomio -> Polinomio -> Bool
equiv l1 l2 = ordena (normaliza l1) == ordena (normaliza l2)