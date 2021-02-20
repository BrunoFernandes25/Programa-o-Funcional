
module Ficha1 where

import Data.Char
-- Exercício 1

perimetro :: Double -> Double
perimetro r = 2 * pi * r
--Double tem mais casas decimais que float 

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2))

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool 
multiplo m n = mod m n == 0

truncaImpar :: [a] -> [a]
truncaImpar l = if (mod (length l) 2 == 0) then l else tail l 

max2 :: Int -> Int -> Int 
-- Ord a => a -> a -> a 
max2 x t = max x t 

-- Ord a => a -> a -> a -> a
max3 :: Int -> Int -> Int -> Int
max3 x t z = max (max2 x t) z

-- Exercício 2

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | delta == 0 = 1 
              | delta > 0 = 2 
              | otherwise = 0 
    where delta = b^2 - 4*a*c
-- (-b +- sqrt (delta)) / 2*a 

raizes :: Float -> Float -> Float -> [Float]
raizes a b c | n == 2 = [x1,x2]
             | n == 1 = [x1]
             | otherwise = [] 
             where n = nRaizes a b c 
                   delta = b^2 - 4 *a*c
                   (x1,x2) = (((-b) + sqrt (delta) / (2*a)), ((-b) - sqrt (delta) / (2*a)))

-- Exercício 3

type Hora = (Int,Int)
-- (0,15) -> Meia noite e um quarto
-- (13,45) -> uma e quarenta e cinco

horaValida :: Hora -> Bool
horaValida (h,m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

horadepois :: Hora -> Hora -> Bool 
horadepois (h1,m1) (h2,m2) = ((h1 == h2) && (m1 < m2)) || (h1 < h2) 

horaconverteminuto :: Hora -> Int
horaconverteminuto (h,m) = h*60 + m 

minutoconvertehora :: Int -> Hora
minutoconvertehora x = (div x 60, mod x 60) 
-- por exemplo 60 minutos = (1,0)

diferencahoras :: Hora -> Hora -> Int
diferencahoras x1 x2 = abs (horaconverteminuto x1 - horaconverteminuto x2) 

adicionaminutos :: Int -> Hora -> Hora
adicionaminutos m h = minutoconvertehora (m + (horaconverteminuto h))

--Exercício 4 

data Hora2 = H Int Int deriving (Show,Eq)
-- H 0 15 -> Meia noire e um quarto
-- H 13 45 -> Uma hora e quarenta e cinco da tarde

horaValida2 :: Hora2 -> Bool 
horaValida2 (H h m) = horaValida (h,m)

horadepois2 :: Hora2 -> Hora2 -> Bool 
horadepois2 (H h1 m1) (H h2 m2) = ((h1 == h2) && (m1 < m2)) || (h1 < h2)

horaconverteminuto2 :: Hora2 -> Int
horaconverteminuto2 (H h m) = h*60 + m

minutoconvertehora2 :: Int -> Hora2
minutoconvertehora2 x = ( H (div x 60) (mod x 60))

diferencahoras2 :: Hora2 -> Hora2 -> Int
diferencahoras2 x1 x2 = abs (horaconverteminuto2 x1 - horaconverteminuto2 x2)

adicionaminutos2 :: Int -> Hora2 -> Hora2
adicionaminutos2 x (H h m) = minutoconvertehora2 (x + (horaconverteminuto2 (H h m))) 

-- Exercício 5 

data Semaforo = Verde | Amarelo | Vermelho  deriving (Show,Eq)

next :: Semaforo -> Semaforo 
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde 

stop :: Semaforo -> Bool 
stop Vermelho = True
stop _ = False  

safe :: Semaforo -> Semaforo -> Bool 
safe x y = if x == Vermelho || y == Vermelho 
    then True
    else False

-- Exercício 6 

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double 
posx (Cartesiano x y) = x
posx (Polar r a) = r * (cos a)
-- em Polar x = r cos x
-- """""""  y = r sin x 

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * (sin a)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r a) = r

angulo :: Ponto -> Double 
angulo (Cartesiano x y) = (atan (y/x))*(180/pi)
angulo (Polar r a) = a

distt :: Ponto -> Ponto -> Double
distt ponto1 ponto2 = sqrt (((posx ponto2 - posx ponto1)^2) + ((posy ponto2 - posy ponto1)^2))

data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
-- um poligono é tipo retangulos, quadradros, losangos, o circulo nao é
poligono (Circulo a b) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices x = case x of
    Circulo _ _ -> []
    Triangulo a b c -> [a,b,c]
    Retangulo a b -> [a,b,c,d]
        where
            c = Cartesiano (posx b) (posy a)
            d = Cartesiano (posx a) (posy b) 

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distt p1 p2
        b = distt p2 p3
        c = distt p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
--area (Retangulo p1 p2) = (distt p1 p3) * altura 
--    where
--        p3 = Cartesiano (posx p2) (posy p1)
--        altura = distt p1 p4
--        p4 = Cartesiano (posx p1) (posy p2)
-- ou 
area (Retangulo p1 p2) = 
    let p3 = Cartesiano (posx p2) (posy p1)
        base = distt p1 p3
        altura = distt p3 p2
    in base * altura
area(Circulo _ r) = pi * r^2

perimetroo :: Figura -> Double
perimetroo (Retangulo a b) = 2 * base + 2 * altura
    where
        base = distt a c
        c = Cartesiano (posx b) (posy a)
        altura = distt c b
--ou
--perimetroo :: Figura -> Double
--perimetroo (Retangulo a b) =
--    let c = Cartesiano (posx b) (posy a)
--        base = distt c b
--        altura = distt a c
--    in 2 * base + 2 * altura 
perimetroo (Triangulo a b c) = x + y + z 
    where 
        x = distt a b
        y = distt b c
        z = distt c a
perimetroo (Circulo _ r) = 2 * r * pi

-- importar o Data.Char
isLowerr :: Char -> Bool
isLowerr x = (ord x) >= 97 && (ord x) <= 122

isDigitt :: Char -> Bool 
isDigitt x = (ord x) >= 48 && (ord x) <= 57

isAlphaa :: Char -> Bool 
isAlphaa x = (isLowerr x) || (isUpperr x)
    where 
        isUpperr x = (ord x) >= 65 && (ord x) <= 90

toUpperr :: Char -> Char 
toUpperr x | isLowerr x = chr (ord x - 32)
           | otherwise = x
-- chr 99 = 'c' por exemplo 

intToDigitt :: Int -> Char 
intToDigitt x = chr (x + 48)

digitToIntt :: Char -> Int
digitToIntt x = (ord x) - 48