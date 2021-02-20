import Ficha1

-- Exercício 1 

type Etapa = (Hora2,Hora2)
type Viagem = [Etapa]

etapabemconstruida :: Etapa -> Bool 
etapabemconstruida (h1,h2) = (horaValida2 h1) && (horaValida2 h2) && (horadepois2 h1 h2)

viagembemconstruida :: Viagem -> Bool 
viagembemconstruida [] = True
viagembemconstruida [e] = etapabemconstruida e 
viagembemconstruida ((h1,h2):(h3,h4):h) = etapabemconstruida (h1,h2) && etapabemconstruida (h2,h3) && viagembemconstruida ((h3,h4):h)
-- viagembemconstruida [(H 1 0, H 2 0),(H 3 30, H 5 0)]

--------------------------------------------------------------------------------------------------------------------------------------------------------
horapartidachegada :: Viagem -> (Hora2,Hora2)
horapartidachegada [] = error "hora invalida"
horapartidachegada [(h1,h2)] = (h1,h2)
horapartidachegada [(h1,h2),(h3,h4)] = (h1,h4)
horapartidachegada ((h1,h2):(h3,h4):h) = horapartidachegada ((h1,h2):h)
-- ou
horapartidachegada2 :: Viagem -> (Hora2,Hora2)
horapartidachegada2 [] = error "hora invalida"
horapartidachegada2 [(h1,h2)] = (h1,h2)
horapartidachegada2 ((h1,h2):h) = (h1, snd (last h))
-- neste caso tinha de ser o segundo porque uma etapa tem a primeira hora e a segunda e a segunda da ultiam hora é que corresponde a hora de chegada
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- supusemos que a viagem ja +e valida claro...
tempoviagemefetiva:: Viagem -> Hora2
tempoviagemefetiva [(h1,h2)] = minutoconvertehora2 (diferencahoras2 h1 h2)
tempoviagemefetiva ((h1,h2):h) = adicionaminutos2 (diferencahoras2 h1 h2) (tempoviagemefetiva h)

tempototalespera :: Viagem -> Hora2
tempototalespera [] = error "se nao há viagem nao há tempo de espera"
tempototalespera [(h1,h2)] = H 0 0
tempototalespera ((h1,h2):(h3,h4):h) = adicionaminutos2 (diferencahoras2 h2 h3) (tempototalespera ((h3,h4):h))

tempototalviagem :: Viagem -> Hora2
tempototalviagem x = minutoconvertehora2 (diferencahoras2 (fst (horapartidachegada2 x)) (snd (horapartidachegada2 x)))

-- Exercício 2

type Poligonal = [Ponto]

complinhapoligonal :: Poligonal -> Double
complinhapoligonal [p1] = 0
complinhapoligonal (p1:p2:p) = distt p1 p2 + complinhapoligonal (p2:p)

poligonalfechada :: Poligonal -> Bool
poligonalfechada p = (length p >= 4) && (head p) == (last p)

--data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:p4:p) = (Triangulo p1 p2 p3):triangula (p1:p3:p4:p)
triangula _ = []

areapoligonal :: Poligonal -> Double
areapoligonal p = somaArea (triangula p)

somaArea :: [Figura] -> Double
somaArea [] = 0.0
somaArea ((Triangulo p1 p2 p3):p) = area (Triangulo p1 p2 p3) + somaArea p
somaArea ((Retangulo p1 p3):p) = area (Retangulo p1 p3) + somaArea p
-- logo...
--somaArea (f:fs) = area f + somaArea fs
-- na areapoligonal de um retangulo -> [(Cartesiano 0 0),(Cartesiano 2 0),(Cartesiano 2 2),(Cartesiano 0 2),(Cartesiano 0 0)] = 3.9999999999999982
-- porque a formula usada para as areas dos triangulos é a formula de heron, senao dava 4 certos.

--mas na soma area da bem porque é a area normal de um retangulo somaArea [Retangulo (Cartesiano 0 0) (Cartesiano 2 2)] = 4.0 


--mover :: Poligonal -> Ponto -> Poligonal 
--mover [] _ = []
--mover (h:t) (Cartesiano x y) = (Cartesiano x y): (mover' t (Cartesiano x y))
--    where 
--        mover' [] _ = [] 
--        mover' ((Cartesiano x y):t) (Cartesiano x' y') = diferencapontos h (Cartesiano x' y'): ()

diferencapontos :: Ponto -> Ponto -> Ponto
diferencapontos (Cartesiano x y) (Cartesiano x' y') =  (Cartesiano (x'-x) (y'-y))

--mover [] a = a
--mover [b] a = a
--mover [(Cartesiano a b),(Cartesiano c d)] (Cartesiano e f) = [(Cartesiano e f),(Cartesiano (c+e-a) (d+f-a))]
--mover ((Cartesiano a b):(Cartesiano e f):t) (Cartesiano c d) = (Cartesiano (c) (d)) : mover ((Cartesiano e f):t) (Cartesiano (e+(c-a)) (f+(d-b)))


--zoom :: Double -> Poligonal -> Poligonal
--zoom 



-- Exercício 3 

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
-- Nao há nomes repetidos e para cada nome existe um contacto

type Nome = String
type Agenda = [(Nome, [Contacto])]

--------------------------------------------------------------------------------------------------------------
--auxiliar
adicionaEmail :: String -> [Contacto] -> [Contacto]
adicionaEmail e [] = [Email e]
adicionaEmail e ((Email h):t) | e == h = (Email h : t)
                              | otherwise = (Email h : adicionaEmail e t) 
adicionaEmail e (h:t) = h: adicionaEmail e t
-- caso o contacto nao seja email adicionamos a lista de contactos

acrescEmail :: Nome -> String -> Agenda -> Agenda 
acrescEmail n e [] = [(n, [Email e])]
-- basicamente se recebemos um nome e um email e nao existe agenda adicionamos isso à agenda
acrescEmail n e ((n',c'):t) | n == n' = (n, adicionaEmail e c') : t -- c' -> contactos
                            | otherwise = (n',c'): acrescEmail n e t
--acrescEmail "AnaSofia" "anasofiagmailcom" [("Bruno", [Tlm 253444444])] =  [("Bruno",[Tlm 253444444]),("AnaSofia",[Email "anasofiagmailcom"])]
--------------------------------------------------------------------------------------------------------------
--auxiliar 
emails :: [Contacto] -> [String]
emails [] = []
emails (Email c :cs) =  c: emails cs
emails (_:cs) = emails cs 

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((n',c):cs) | n == n' = Just (emails c)
                        | otherwise = verEmails n cs
--verEmails "sofia" [("ze",[Email "zegmail"]),("sofia",[Email "sofiagmail",Email "ghgjg"])] =  Just ["sofiagmail","ghgjg"]

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Tlm c):cs) = fromIntegral (c) : consTelefs cs   
consTelefs ((Trab c):cs) = fromIntegral (c) : consTelefs cs
consTelefs ((Casa c):cs) = fromIntegral (c) : consTelefs cs
consTelefs ((Email c):cs) = consTelefs cs
-- vai nos dar todos os numeros telefonicos de uma agenda sejam fixos ou móveis 
--ou uma oura função 
consTelefs2 :: [Contacto] -> [Integer]
consTelefs2 [] = []
consTelefs2 (c:cs) = case c of
                    Tlm   c -> fromIntegral (c) : consTelefs2 cs 
                    Trab  c -> fromIntegral (c) : consTelefs2 cs
                    Casa  c -> fromIntegral (c) : consTelefs2 cs           
                    Email c -> consTelefs2 cs



casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing 
casa n [(n',(c:cs))] | n == n' = case c of 
                                Casa c -> Just c 
                                Trab c -> casa n [(n',cs)]
                                Tlm c -> casa n [(n',cs)]
                                Email c -> casa n [(n',cs)]

-- Exercício 4

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((n',d):t) | n == n' = Just d
                     | otherwise = procura n t
--procura "Zequinha" [("Zeze",D 13 12 1980),("Zequinha",D 1 4 1999),("Zezao",D 5 5 2003)] =  Just (D 1 4 1999)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade d n [] = Nothing
idade d n ((n',d'):t) | n == n' = Just (calculaidade d d')
                      | otherwise = idade d n (t)
-- idade (D 10 10 2020) "Bruno" [("Ze",D 10 03 1988),("Rita",D 07 08 2000),("Bruno",D 25 09 2002)] = Just 18

calculaidade :: Data -> Data -> Int 
calculaidade (D d m a) (D d' m' a') | m > m' || ((m == m') && (d >= d')) = (a -a')
                                    | otherwise = (a - a' - 1)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d' m' a') | a < a' = True
                                | (a == a') && ((m < m') || (m == m') && (d < d')) = True 
                                | otherwise = False

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = insere (n,d) (ordena t)
    where 
        insere (n,d) [] = [(n,d)]
        insere (n,d) ((n',d'):t) | anterior d' d = (n',d'): insere (n,d) t
                                 | otherwise = (n,d):(n',d'):t
-- a debaixo nao ta bem e é mais confusa
--ordena2:: Data -> TabDN -> [(Nome,Int)]
--ordena2 _ [] = []
--ordena2 (D d m a) tabela = (n,idade) : ordena2 (D d m a) ts
--    where ((n,D dx mx ax):ts) = ordena tabela
--          idade = if m > mx || mx == m && d > dx then (a - ax) else ((a - ax) - 1)

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade (D d m a) [] = [] 
porIdade d ((n,d'):t) = ordena3 ((n,calculaidade d d'): porIdade d t) 
    where 
        ordena3 [] = []
        ordena3 [(n,x)] = [(n,x)]
        ordena3 ((n,x):(n',x'):t) | x' < x = (n',x'): ordena3 ((n,x):t)
                                  | x' >= x = (n,x):(n',x'):t  


-- Exercício 5

data Movimento = Credito Float | Debito Float deriving Show

--data Data = D Int Int Int deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show
-- valores sao sempre positivos

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext n []) _ = []
extValor (Ext n ((_,_,mov):t)) x' = case mov of 
                                    Credito x -> if x > x' then mov: extValor (Ext x t) x' else extValor (Ext x t) x'
                                    Debito  x -> if x > x' then mov: extValor (Ext x t) x' else extValor (Ext x t) x'
-- extValor (Ext 100 [(D 1 1 2000,"Ze",Debito 50),(D 2 2 2000,"Ze",Credito 100)]) 25 = [Debito 50.0,Credito 100.0]

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext n []) y = []
filtro (Ext n ((n',d,mov):t)) y = if elem d y then (n',mov): (filtro (Ext n t) y) else (filtro (Ext n t) y)
--filtro (Ext 1 [(D 1 1 2020,"Ana",Debito 50),(D 2 2 2019,"Bruno",Credito 45),(D 3 3 2000, "Carlos",Credito 89)]) ["Ana","Bruno"]
-- = [(D 1 1 2020,Debito 50.0),(D 2 2 2019,Credito 45.0)]

-- semore que se trabalha com pares importante forma da expressao !!!
creDeb :: Extracto -> (Float,Float)
creDeb (Ext n ((_,_,Credito x):t)) = (x+a,b)
    where 
        (a,b) = creDeb (Ext n t)
creDeb (Ext n ((_,_,Debito x):t)) = (a,x+b)
    where 
        (a,b) = creDeb (Ext n (t))
creDeb _ = (0,0)
-- (Ext 5 [(D 2 2 2002,"Quim",Credito 300),(D 23 03 1999,"Nelinha",Debito 150),(D 5 5 1143,"Afonso", Debito 100)])
-- = (300.0,250.0)

saldo :: Extracto -> Float
saldo (Ext n []) = 0
saldo (Ext n t) = (n+x-y)
    where (x,y) = creDeb (Ext n t) 
-- saldo (Ext 500 [(D 1 1 1999,"Tony",Credito 250),(D 5 12 2000,"Tony",Debito 300)]) = 550.0
-- debito -> (divida) e subtrai, credito -> (emprestimo) e soma!!!