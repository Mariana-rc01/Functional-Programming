module Ficha1 where
import Data.Char

-- Exercício 1: Confirmada na aula 

-- a) Confirmada na aula

perimetro' :: Float -> Float
perimetro' r = pi*2*r

-- b) Confirmada na aula

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x, y) (a, b) = sqrt((x - a)^2 + (y - b)^2)

-- c) Confirmada na aula

primUlt :: [a] -> (a,a)
primUlt lista = (head lista, last lista)

-- d) Confirmada na aula

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

-- e) Confirmada na aula
-- $ significa aplicar à função

truncaImpar :: [a] -> [a]
truncaImpar lista
    | odd $ length(lista) = tail lista
    | otherwise           = lista

-- ou

truncaImpar' :: [a] -> [a]
truncaImpar' l = if mod (length l) 2 == 0 then l else tail l

-- f) Confirmada na aula

max2 :: Int -> Int -> Int
max2 x y | x > y = x
         | otherwise = y

-- g) Confirmada na aula

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

-- Exercício 2: Confirmada na aula

-- a) b^2 - 4 a c Confirmada na aula

nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c
    | binomio == 0 = 1
    | binomio > 0 = 2
    | binomio < 0 = 0
    where binomio = b^2 - 4*a*c

-- b) -b +/- binomio / 2a Confirmada na aula

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | n == 0 = []
    | n == 1 = [x]
    | n == 2 = [x,y]
    where n = nRaizes a b c
          binomio = b^2 - 4*a*c
          x = (-b + sqrt(binomio))/2*a
          y = (-b - sqrt(binomio))/2*a


-- Exercício 3:

type Hora' = (Int,Int)

-- a) Confirmada na aula

horaval :: Hora' -> Bool
horaval (x,y) = (0<=x && x<24) && (0<=y && y<60)

-- b) Confirmada na aula

horamaior :: Hora' -> Hora' -> Hora'
horamaior (x,y) (z,w) = if horaval (x,y) && horaval (z,w)
                            then if x<z || (x==z && y<w) then (z,w) else (x,y)
                        else error "Hora não é válida"

-- c) Confirmada na aula

convH :: Hora' -> Int
convH (x,y) = x*60+y

-- d) div w 60 vai dar a parte inteira que são as horas 
--e mod w 60 vai dar o resto da divisão que serão os minutos
-- Confirmada na aula

convMin :: Int -> Hora'
convMin w = (div w 60, mod w 60)

-- e) 

difHs :: Hora' -> Hora' -> Int
difHs (x,y) (w,z) = if horaval (x,y) && horaval (w,z)
                        then convH (abs(x-w), abs(y-z))
                    else error "Hora não é válida"

-- f)

addMin :: Int -> Hora' -> Hora'
addMin w (x,y) = convMin (convH (x,y) + w)

-- Exercício 4:
-- data - define um tipo novo com base em tipos pré-existentes, não preocupar com o deriving (Show,Eq)

data Hora = H Int Int deriving (Show,Eq)

-- a) Confirmada na aula

horaval' :: Hora -> Bool
horaval' (H x y) = (0<=x && x<24) && (0<=y && y<60)

-- b) Confirmada na aula

horamaior' :: Hora -> Hora -> Hora
horamaior' (H x y) (H z w) = if horaval' (H x y) && horaval' (H z w)
                            then if x<z || (x==z && y<w) then (H z w) else (H x y)
                        else error "Hora não é válida"

-- c)

convH' :: Hora -> Int
convH' (H x y) = x*60+y

-- d)

convMin' :: Int -> Hora
convMin' w = H (div w 60) (mod w 60)

-- e)

difHs' :: Hora -> Hora -> Int 
difHs' (H x y) (H w z) = if horaval' (H x y) && horaval' (H w z)
                        then abs (convH' (H x y) - convH' (H w z))
                    else error "Hora não é válida"

-- f)

addMin' :: Int -> Hora -> Hora
addMin' w (H x y) = convMin' (convH' (H x y) + w)


-- Exercício 5:

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) Confirmada na aula

next :: Semaforo -> Semaforo
next estado 
            | estado == Verde = Amarelo
            | estado == Amarelo = Vermelho
            | estado == Vermelho = Verde

-- b) 

stop :: Semaforo -> Bool
stop estado = estado == Vermelho

-- c) V V, A V, V A

safe :: Semaforo -> Semaforo -> Bool
safe e1 e2 = e1 == Vermelho || e2 == Vermelho

-- Exercício 6: Confirmada na aula

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- a) Confirmada na aula

posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x y -> x
                           Polar z w -> if w == pi/2 then 0 else z * cos w

-- b) Confirmada na aula

posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano x y -> y
                           Polar z w -> if w == pi then 0 else z * sin w

-- c) Confirmada na aula

raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> dist (x,y) (0,0)
                           Polar z w -> z

-- d) Confirmada na aula

angulo :: Ponto -> Double
angulo ponto = case ponto of Cartesiano x y -> if x<0 && y<0 then pi else
                                               if x<0 then pi + atan (y/x) else atan(y/x)
                             Polar z w -> w

-- e) Confirmada na aula

dist' :: Ponto -> Ponto -> Double
dist' p1 p2 = sqrt ((posy p1 - posy p2)^2 + (posx p1 - posx p2)^2)

-- Exercício 7: Confirmada na aula
--Circulo - ponto e raio; Rectangulo - vértices da diagonal; Triangulo - vértices
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

-- para testarmos uma função invés de etsra sempre a criar uma figura, podemos simplesmente definir as figuras aqui:

circ :: Figura
circ = Circulo (Cartesiano 0 0) 3.5

rect :: Figura
rect = Rectangulo (Cartesiano 0 0) (Cartesiano 2 2)

-- a) Confirmada na aula

poligono :: Figura -> Bool
poligono (Circulo _ _)        = False
poligono (Rectangulo p1 p2)   = posy p1 /= posy p2 && posx p1 /= posx p2
poligono (Triangulo p1 p2 p3) = (posy p1 - posy p2)/(posx p1 -posx p2) /= (posy p2 - posy p3)/(posx p2 - posx p3)

-- ou - Caso seja Circulo, não é polígono; caso contrário, é polígono

poligono' :: Figura -> Bool
poligono' (Circulo _ _) = False
poligono' _             = True

-- b) Confirmada na aula

vertices :: Figura -> [Ponto]
vertices (Circulo _ _)        = []
vertices (Rectangulo p1 p2)   = if poligono (Rectangulo p1 p2) 
                                then [p1, p2, (Cartesiano (posx p1) (posy p2)), (Cartesiano (posx p2) (posy p1))] else []
vertices (Triangulo p1 p2 p3) = if poligono (Triangulo p1 p2 p3) then [p1, p2, p3] else []

-- c) Confirmada na aula

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p1 p2) = abs(posx p1 - posx p2) * abs(posy p1 - posy p2)
area (Circulo _ r) = pi * r^2

-- d) Confirmada na aula

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2*pi*r
perimetro (Rectangulo p1 p2) = abs(posx p1 - posx p2)*2 + abs(posy p1 - posy p2)*2
perimetro (Triangulo p1 p2 p3) = 
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
    in a+b+c

-- Exercício 8:

-- a) ord :: Char -> Int e chr :: Int -> Char
-- Confirmada na aula

isLower' :: Char -> Bool
isLower' c = elem c ['a'..'z']

-- ou 

islower :: Char -> Bool
islower c = ord c >= 97 && ord c <=122
-- b)

isDigit' :: Char -> Bool
isDigit' c = elem c ['0'..'9']

-- c)

isAlpha' :: Char -> Bool
isAlpha' c = isLower' c || elem c ['A'..'Z']

-- d) diferença de 32 dígitos

toUpper' :: Char -> Char
toUpper' c = if isLower' c then chr (ord c - 32) else c

-- e)

intToDigit' :: Int -> Char
intToDigit' x = if x>=0 && x<=9 then chr (x+48) 
              else error "Número inválido"

-- f)

digitToInt' :: Char -> Int
digitToInt' c = ord (c) - 48
