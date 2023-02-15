
-- 1)

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a > b = []
                | otherwise = a : (enumFromTo' (a+1) b)

-- 2) start > end && next >= start || start < end && next < start

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a c b | a > b  && c >= a || c < a && a < b = []
                      | otherwise = a : enumFromThenTo' c (2*c - a) b

-- 3)

concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (h:t) l = h : (concatena t l)

-- 4)

pos :: [a] -> Int -> a
pos (h:t) a | a == 0 = h
            | otherwise = pos t a

-- 5)

reversee :: [a] -> [a]
reversee [] = []
reversee l = (last l) : reversee (init l)

-- 6)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _  = []
take' a (h:t) | a < 0 = []
              | otherwise = h : (take' (a-1) t)

-- 7)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' a (h:t) | a < 0 = h:t
              | otherwise = drop' (a-1) t

-- 8)

zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (h:t) (xs:ys) = ((h,xs):zipp t ys)

-- 9)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' a h | a <= 0 = []
               | otherwise = (h:replicate' (a-1) h)

-- 10)

intersperce' :: a -> [a] -> [a]
intersperce' _ []  = []
intersperce' _ [h] = [h]
intersperce' a (h:t) = (h:a:intersperce' a t)

-- 11)

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (h:t) | elem h hh = (h:hh):tt
             | otherwise = [h]:hh:tt
             where (hh:tt) = group' t

-- 12)

concatt :: [[a]] -> [a]
concatt [] = []
concatt (h:t) = h ++ concatt t

-- 13)

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits'(init l) ++ [l]

-- 14)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (h:t) = [(h:t)] ++ (tails' (t))

-- 15)

heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = (head h : heads t)

-- 16)

total :: [[a]] -> Int
total [] = 0
total (h:t) = (length h) + (total t)

-- 17)

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z):(fun t)

-- 18)

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((hx,hy,hz):t) = hx ++ cola t

-- 19)

idade :: Int -> Int -> [(String, Int)] -> [String]
idade _ _ [] = []
idade ano i ((nome,nasci):t) | ano - nasci > i = (nome : (idade ano i t))
                             | otherwise = idade ano i t

-- 20)

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                  | otherwise = []

-- 21)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = let aux x (h:t) = if mod x h == 0 then False else aux n t
                aux _ [] = True
            in aux n [2 .. ceiling (sqrt (fromIntegral n))]

-- 22)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (hh:tt) = h == hh && isPrefixOf' t tt

-- 23)

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l (h:t) = l == (h:t) || isSuffixOf' l t

-- 24)

isSubsquenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsquenceOf' [] _ = True
isSubsquenceOf' _ [] = False
isSubsquenceOf' (h:t) (hh:tt) = (h==hh && isSubsquenceOf' t tt) || isSubsquenceOf' (h:t) tt

-- 25)

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' a (h:t) | a == h = 0: map (+1) (elemIndices' a t)
                     | otherwise = map (+1) (elemIndices' a t)

-- 26)

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if elem h t then nub' t else h:nub' t

-- 27)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (h:t) = if a ==h then t else h:delete' a t

-- 28)

remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove (h:t) (x:y) | h == x = remove t y
                   | otherwise = h:remove t (x:y)

-- 29)

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t) = if elem h l then union' l t else union' (l ++ [h]) t

-- 30)

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) l = if elem h l then h:intersect' t l else intersect' t l

-- 31)

insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t) | a>h = h:insert' a t
                | otherwise = a:h:t

-- 32)

unwordss :: [String] -> String
unwordss [] = ""
unwordss (h:t) = h ++ (if length t == 0 then "" else " ") ++ unwordss t


-- 33)

unliness :: [String] -> String
unliness [] = ""
unliness (h:t) = h ++ "\n" ++ unliness t

-- 34)

pMaior :: Ord a => [a] -> Int
pMaior [a] = 0
pMaior (h:t) | h >= (t !! ts) = 0
             | otherwise = 1 + ts
             where ts = pMaior t

-- 35)

lookupp :: Eq a => a -> [(a,b)] -> Maybe b
lookupp _ [] = Nothing
lookupp a ((x,y):t) | a == x = Just y
                    | otherwise = lookupp a t

-- 36)

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:t) | h < head t = h:preCrescente t
                   | otherwise = [h]

-- 37)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insere h (iSort t)
            where inserte a (h:t) | a>h = h:inserte a t
                                  | otherwise = a:h:t
                  insere a [] = [a]

-- 38)

menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (h:t) (x:y) | h < x = True
                  | h == x = menor t y
                  | otherwise = False

-- 39)

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a (h:t) = if a == fst h then True else elemMSet a t

-- 40)

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h1,h2):t) = replicate h2 h1 ++ converteMSet t

-- 41)

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((h1,h2):t) = if a == h1 then ((h1,h2+1):t) else ((h1,h2):insereMSet a t)

-- 42)

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((h1,h2):t) | a == h1 && h2>1 = (h1,h2-1):t
                         | a == h1 && h2 == 1 = t
                         | otherwise = (h1,h2):removeMSet a t

-- 43)

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = reverse(inseree h (constroiMSet t))
                    where inseree a ((h1,h2):t) = if a == h1 then ((h1,h2+1):t) else ((h1,h2):inseree a t)
                          inseree a [] = [(a,1)]

-- 44)

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a:as,bs)
                            where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b:bs)
                            where (as,bs) = partitionEithers t

-- 45)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just a:t) = a:catMaybes t

-- 46)

data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi > xf = Oeste:caminho (xi-1,yi) (xf,yf)
                        | xi < xf = Este:caminho (xi+1,yi) (xf,yf)
                        | yi > yf = Sul:caminho (xi,yi-1) (xf,yf)
                        | yi < yf = Norte:caminho (xi,yi+1) (xf,yf)
                        |otherwise = []

-- 47) Ver se ponto é igual ao último, se não for retira o último e vê o penúltimo

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (h:t) = case h of Norte -> posicao (x,y+1) t
                                Sul   -> posicao (x,y-1) t
                                Este  -> posicao (x+1,y) t
                                Oeste -> posicao (x-1,y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops ponto (h:t) = ponto == posicao ponto (h:t) || hasLoops ponto (init (h:t))

-- 48)
type Ponto = (Float,Float)

data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1) (x2,y2)):t) | abs(y1-y2)==abs(x1-x2) = 1 + contaQuadrados t
                                          | otherwise = contaQuadrados t

-- 49)

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = c*l + areaTotal t
                                     where c = abs(x1-x2)
                                           l = abs(y1-y2)

-- 50)

data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of Bom      -> 1 + naoReparar t
                             Razoavel -> 1 + naoReparar t
                             Avariado -> naoReparar t