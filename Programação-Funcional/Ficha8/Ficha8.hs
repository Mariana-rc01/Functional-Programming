module Ficha8 where

import Data.List
import Data.Char

-- Exercício 1:

data Frac = F Integer Integer

-- a)

normaliza :: Frac -> Frac
normaliza (F x y) | y < 0 = normaliza (F  (-x) (-y))
                  | otherwise = let d = mdc x y
                                in F (div x d) (div y d)

mdc :: Integer -> Integer -> Integer
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y (mod x y)

-- b)

instance Eq Frac where
    (F a b) == (F c d) = a * d == c * b

-- c)

instance Ord Frac where
    (F a b) <= (F c d) = a * d <= c * b

-- d)

instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

-- e)

instance Num Frac where
    (F a b) + (F c d) = F (a*d + b *c) (b*d)
    (F a b) * (F c d) = F (a*c) (b*d)
    x - y = x + negate y

    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F (signum a * signum b) 1
    fromInteger x = F x 1

-- f)

seleciona :: Frac -> [Frac] -> [Frac]
seleciona f l = filter (\x -> (F 2 1) * f < x) l 

-- ou

select :: Frac -> [Frac] -> [Frac]
select = filter . (<) . (2 *)

-- Exercício 2:

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


-- a)

instance Show a => Show (Exp a) where
    show (Const n) = show n
    show (Simetrico e) = "(-(" ++ show e  ++ "))"
    show (Mais e1 e2)  = "("   ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Menos e1 e2) = "("   ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (Mult e1 e2)  = "("   ++ show e1 ++ "*" ++ show e2 ++ ")"

-- b)

valorExp :: Num a => Exp a -> a
valorExp (Const n) = n
valorExp (Simetrico e) = negate (valorExp e)
valorExp (Mais e1 e2)  = valorExp e1 + valorExp e2
valorExp (Menos e1 e2) = valorExp e1 - valorExp e2
valorExp (Mult e1 e2)  = valorExp e1 * valorExp e2

instance (Num a,Eq a) => Eq (Exp a) where
    a == b = valorExp a == valorExp b 

-- c)

instance (Ord a,Num a) => Num (Exp a) where
   x + y = Mais x y
   x - y = Menos x y
   x * y = Mult x y

   negate (Simetrico x) = x
   negate x = Simetrico x
   fromInteger x = Const (fromInteger x)

   abs (Const x) = Const (abs x)
   abs (Simetrico x) = abs x
   abs x = if valorExp x < 0 then negate x else x

   signum x | valorExp x < 0 = Const (-1)
            | valorExp x == 0 = Const 0
            | valorExp x > 0 = Const 1


-- Exercício 3:

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)] -- saldo inicial

-- a)

instance Eq Data where 
    (D dia1 mes1 ano1) == (D dia2 mes2 ano2) = dia1 == dia2 && mes1 == mes2 && ano1 == ano2

instance Ord Data where
    (D dia1 mes1 ano1) <= (D dia2 mes2 ano2) = ano1 <= ano2 || (ano1 == ano2 && mes1 <= mes2) || (ano1 == ano2 && mes1 == mes2 && dia1 <= dia2)

-- b)

instance Show Data where
    show (D dia mes ano) = show ano ++ "/" ++ show mes ++ "/" ++ show dia

-- c)

ordena :: Extracto -> Extracto
ordena (Ext saldo l) = Ext saldo (sortOn (\(x,y,z) -> x) l)

-- d) 0000-00-00 (11 espaços para a data no máximo)
-- para a descrição: maximo length de todas as strings (Descricao - 9 letras, Credito - 7 letras)

instance Show Extracto where
    show ext = "Saldo anterior" ++ show saldoI ++ 
               "\n---------------------------------------" ++
               "\nData       Descricao" ++ replicate (descMax - 9) ' ' ++ "Credito" ++ replicate (credMax - 7) ' ' ++ "Debito" ++
               "\n---------------------------------------\n" ++
               unlines (map (\(dat,desc,mov) -> show dat ++ replicate (11-length(show dat)) ' '
                                         ++ map toUpper desc ++ replicate (descMax - length desc) ' ' 
                                         ++ case mov of Credito quant -> show quant ++ replicate (credMax - length (show quant)) ' '
                                                        Debito  quant -> replicate credMax ' ' ++ show quant) l) ++
               "---------------------------------------" ++
               "\nSaldo actual: " ++ show (saldo ext)
               where (Ext saldoI l) = ordena ext
                     descMax = max (length "Descricao   ") (maximum $ map (\(_,desc,_) -> length desc) l)
                     credMax = max (length "Credito  ") (maximum $ map (\(_,_,mov ) -> case mov of Credito x -> length (show x) ; _ -> 0) l)

-- Funções da Ficha3:
saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext saldo ((date,descri,mov):t)) = saldo + fst credito - snd credito
                                        where credito = creDeb (Ext saldo ((date,descri,mov):t))

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext saldo ((_,_,mov):t)) = case mov of Credito c -> (total1+c,total2)
                                               Debito d -> (total1,d+total2)
                               where (total1,total2) = creDeb (Ext saldo t)