module Aula1 where
import Data.Char
import Data.String

{- Exercício 1:

Prelude> :type length
length :: Foldable t => t a -> Int

Prelude> :t tail
tail :: [a] -> [a]

Prelude> :t init (retira o último elemento da lista)
init :: [a] -> [a]

Prelude> :t last
last :: [a] -> a

Prelude> :t elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool

Prelude> :t (++)
(++) :: [a] -> [a] -> [a]

Prelude> :t div
div :: Integral a => a -> a -> a

Prelude> :t mod
mod :: Integral a => a -> a -> a

Prelude> :t fst
fst :: (a, b) -> a

Prelude> :t snd
snd :: (a, b) -> b
-}

-- Exercício 2:

-- a)

aQuad :: Float -> Float
aQuad l = l*l

-- b)

pRec :: Float -> Float -> Float
pRec l c = 2*l + 2*c

-- c) vChar 'x' "abscx"

vChar :: Char -> String -> Bool
vChar x y = elem x y

-- d)

fD :: [a] -> [a]
fD lista = if mod (length(lista)) 2 == 0 then tail(lista) else init(lista)

-- e)

primult :: [a] -> (a,a)
primult l = (head l, last l)

-- f)

lNomes :: [String] -> (String,String)
lNomes l = (head l, last l)

-- g)

parListas :: ([a],[a]) -> (a,[a])
parListas (xs,ys) = (head xs,ys)

-- h) head (head l) - devolve o caracter da lista de strings, caso façamos take 1 lista devolve a lista com o caracter

lNames :: [String] -> String
lNames l = take 1 (head l) ++ "." ++ last l

-- Exercício 3: feito na ficha1.hs