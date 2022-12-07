module Ficha3 where

import Ficha1

-- Exercício 1:

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- a) Confirmada na aula

etapabemconstr :: Etapa -> Bool
etapabemconstr ((H x y), (H x1 y1)) = (horaval' (H x y) && horaval' (H x1 y1)) && (horamaior' (H x y) (H x1 y1)) == (H x1 y1)

-- b) Confirmada na aula

viagembem  :: Viagem -> Bool
viagembem [h] = etapabemconstr h
viagembem (h1:h2:t) = etapabemconstr h1 && etapabemconstr h2 && viagembem (h2:t)

-- c) Confirmada na aula

horaPartidaChegada :: Viagem -> (Hora,Hora)
horaPartidaChegada l = (fst(head l), snd(last l))

-- Desnecessário, mais fácil a linha de cima
-- horaPartidaChegada [h] = h
-- horaPartidaChegada ((h1,_):(_,h2):t) = horaPartidaChegada ((h1,h2):t)

-- d) Confirmada na aula

tempoViagem :: Viagem -> Hora
tempoViagem [(h1,h2)] = convMin' (difHs' h1 h2)
tempoViagem ((h1,h2):t) = addMin'(difHs' h1 h2)(tempoViagem t)

-- ou

tempoEtapa:: Etapa -> Int
tempoEtapa (hi,hf) = convH' hf - convH' hi

tempoAViajar :: Viagem -> Int
tempoAViajar [] = 0
tempoAViajar (e:es) = tempoEtapa e + tempoAViajar es

-- e) Confirmada na aula

tempoEspera :: Viagem -> Hora
tempoEspera [(h1,h2)] = H 0 0
tempoEspera ((h1,h2):(h3,h4):t) = addMin' (difHs' h2 h3) (tempoEspera ((h3,h4):t))

-- ou

tempoAEsperar :: Viagem -> Int
tempoAEsperar [] = 0
tempoAEsperar [_] = 0
tempoAEsperar ((_,f):e2@(c,_):t) = (convH' c - convH' f) + (tempoAEsperar (e2:t))

-- f) Confirmada na aula

tempoTotal :: Viagem -> Hora
tempoTotal l = addMin' (convH' (tempoViagem l)) (tempoEspera l)

-- ou

tempototal l = tempoAEsperar l + tempoAViajar l

-- ou

tempototal' l = let (hi,hf) = horaPartidaChegada l
                in tempoEtapa (hi,hf)

-- Exercício 2:

type Poligonal = [Ponto]

-- a)

compLinha :: Poligonal -> Double
compLinha [] = 0
compLinha l@(h:t) | length l == 1 = 0
                  | otherwise = (dist' h (head t)) + compLinha t

-- b) Para ser uma linha fechada tem que ter no mínimo 3 linhas.

linhaFechada :: Poligonal -> Bool
linhaFechada l = length l >= 3 && head l == last l

-- c)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps) = if p1 /= p3 then (Triangulo p1 p2 p3):triangula (p1:p3:ps) else []
triangula _ = []

-- d)

areaDelimitada :: Poligonal -> Double
areaDelimitada [] = 0
areaDelimitada (p1:p2:p3:ps) = area (head (triangula (p1:p2:p3:ps))) + areaDelimitada ps

-- e)

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover (h:t) p = p:t

-- f) O ponto inicial mantém-se, mas o resto é multiplicado pelo factor de escala

zoom :: Double -> Poligonal -> Poligonal
zoom a [p1,(Cartesiano x y)] = [p1, (Cartesiano (a*x) (a*y))]
zoom a (p1:(Cartesiano x y):t) = p1: (zoom a (p2:t))
                               where p2 = Cartesiano (x*a) (y*a)
zoom _ p = p

-- Exercício 3:

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email ((h1,h2):t) | nome == h1 = ((h1,h2 ++ [Email email]):t)
                                   | otherwise =  (h1,h2):acrescEmail nome email t

-- b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(h1,h2)] = if nome == h1 then Just (map(\(Email e)-> e) h2) else Nothing
verEmails nome ((h1,h2):t) = if nome == h1 then verEmails nome [(h1,h2)] else verEmails nome t

-- c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of Casa n  -> n : consTelefs t
                             Trab n  -> n : consTelefs t
                             Tlm  n  -> n : consTelefs t
                             _       -> consTelefs t

-- d)

casa :: Nome -> Agenda -> Maybe Integer
casa nome [(h1,h2)] = if nome == h1 then Just (head(map(\(Casa a) -> a) h2)) else Nothing
casa nome ((h1,h2):t) = if nome == h1 then casa nome [(h1,h2)] else casa nome t

-- Exercício 4:

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String (já está em cima definido)

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

-- a) Confirmada na aula

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((h1,h2):t) = if nome == h1 then Just h2 else procura nome t

-- b) Se o mes for depois do mês de aniversário, então é a diferença correta.
-- Se for antes tem que ser menos um ano dessa diferença.
-- Se for no mês de aniversário e num dia depois, então a diferença é correta,
-- Se for no mês de aniversário e antes do dia de aniversário, tem que ser menos um ano.
-- Caso ainda não tenha nascido não retorna nada
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade date@(D dia1 mes1 ano1) nome ((h1,D dia mes ano):t)
      | nome == h1 && (mes1>mes || (mes1 == mes && dia1>=dia)) = Just (ano1-ano)
      | nome == h1 && mes1<mes = Just (ano1-ano - 1)
      | nome == h1 && ano1 > ano = Nothing
      | otherwise = idade date nome t

-- c) se data 1 é inferior à data 2: ano1<ano2 ou 
-- ano1 == ano2 && mes1<mes2 ou
-- ano1==ano2 && mes1==mes2 && dia1<dia2 

anterior :: Data -> Data -> Bool
anterior (D dia1 mes1 ano1) (D dia2 mes2 ano2) =
          ano1<ano2 || (ano1 == ano2 && mes1<mes2) || (ano1==ano2 && mes1==mes2 && dia1<dia2)

-- d)

ordena :: TabDN ->TabDN
ordena [] = []
ordena ((nome,dia):t) = insere (nome,dia) (ordena t)
                      where insere x [] = [x]
                            insere (n,d) ((ns,ds):t) | anterior ds d = (ns,ds):insere (n,d) t
                                                     | otherwise = (n,d):(ns,ds):t

-- e)

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade date@(D dia1 mes1 ano1) tabela = porIdade date t ++ [(nome,idade)]
            where ((nome,(D dia mes ano)):t) = ordena tabela
                  idade= if mes1>mes || (mes1 == mes && dia1>=dia) then ano1-ano else ano1-ano-1

-- Exercício 5:

data Movimento = Credito Float | Debito Float deriving Show

data Dataa = Dd Int Int Int deriving Show

data Extracto = Ext Float [(Dataa, String, Movimento)] deriving Show
--          saldo inicial (data da operação, descrição, quantia movimentada>0)

-- a)


extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext si ((_,_,mov):t)) valor = 
         case mov of Credito c -> if c>valor then mov : extValor (Ext si t) valor else extValor (Ext si t) valor
                     Debito d  -> if d>valor then mov : extValor (Ext si t) valor else extValor (Ext si t) valor

-- b)

filtro :: Extracto -> [String] -> [(Dataa,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext _ _) [] = []
filtro (Ext saldo ((date,desc,mov):ts)) lista = if elem desc lista then (date,mov):(filtro (Ext saldo ts) lista)
                                                                   else filtro (Ext saldo ts) lista

-- c)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext saldo ((_,_,mov):t)) = case mov of Credito c -> (total1+c,total2)
                                               Debito d -> (total1,d+total2)
                               where (total1,total2) = creDeb (Ext saldo t)

-- d)

saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext saldo ((date,descri,mov):t)) = saldo + fst credito - snd credito
                                        where credito = creDeb (Ext saldo ((date,descri,mov):t))