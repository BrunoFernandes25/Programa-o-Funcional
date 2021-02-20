import Data.List
import Data.Char

-- Exercício 1

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
-- funA [2,3,5,1] = 2^2 + funA [3,5,1]
--                  = 4 + 3^2 + funA [5,1]
--                  = 4 + 9 + 5^2 + funA [1]
--                  = 13 + 25 + 1^2 + funA []
--                  = 38 + 1 + 0
--                  = 39

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t) else (funB t)
-- fun [8,5,12] = 8 : funB [5,12] 
--              = 8 : funB [12]
--              = 8 : 12 : funB []
--              = 8 : 12 : []
--              = [8,12]

funC (x:y:t) = funC t
funC [x] = []
funC [] = []
-- funC [1,2,3,4,5] = funC (1:2:[3,4,5]) 
--                  = funC [3,4,5]
--                  = funC (3:4:[5])
--                  = funC [5] 
--                  = []

funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t 
-- funD "otrec" = g [] "otrec" 

-- Exercício 2

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2 * h) : dobros t 

numOcorre :: Char -> String -> Int
numOcorre x [] = 0 
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t
-- numOcorre 'x' ['x','s','c','c','x','x']
--3

positivos :: [Int] -> Bool 
positivos [] = True
positivos (h:t) | h > 0 = positivos t
                | otherwise = False
-- ou 
--positivos1 :: [Int] -> Bool
--positivos1 = all (> 0)
-- dar import ao Data.List

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0 then h: soPos t else soPos t

somaNeg :: [Int] -> Int 
somaNeg [] = 0
somaNeg (h:t) = if h < 0 then h + somaNeg t else somaNeg t 

--somaNeg' :: [Int] -> Int
--somaNeg' = foldl (\acc x -> if x < 0 then acc + x else acc) 0
-- rever depois quando souber mais materia 

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l = if (length l <= 3) then l else tresUlt (tail l) 
-- dar import ao data.list

segundos :: [(a,b)] -> [b]
segundos [(x,y)] = [y]
segundos ((h,t):p) = t: segundos p 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 
nosPrimeiros x [] = False
nosPrimeiros x ((h,t):p) = if x == h then True else nosPrimeiros x p

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(x,y,z):p) = sumTriplos ((a+x, b+y, c+z):p)

-- Exercício 3 
-- import ao Data.Char
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if h >= '0' && h <= '9' then h: soDigitos t else soDigitos t 

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if (ord h) >= 97 && (ord h) <= 122 
                        then 1 + minusculas t 
                        else minusculas t
-- ou depois do if meter (isLower h) que tem no Data.Char

-- [Char] = String ---------
nums :: String -> [Int]
nums [] = []
nums (h:t) = if ((ord h) >= 48) && ((ord h) <= 57) then digittoInt h :nums t else nums t
-- ou depois do if meter (isDigit h) que tem no Data.Char
digittoInt :: Char -> Int
digittoInt x = (ord x) - 48  

-- Exercício 4 

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0 
conta n ((h,t):p) = if n == t then 1 + conta n p else conta n p   

grau :: Polinomio -> Int 
grau [(a,b)] = b
grau ((a,b):(c,d):p) | b < d = grau ((c,d):p) 
                     | otherwise = grau ((a,b):p) 

selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = []
selgrau x ((h,t):p) = if x == t then (h,t): selgrau x p else selgrau x p 
-- selgrau 2 [(1,2),(3,4),(3,2)]
-- [(1.0,2),(3.0,2)]

deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((h,t):p) = if t > 0 then (h*fromIntegral t,t-1): deriv p else deriv p
-- nao podia fazer a derivada a multiplicar um Int com um Float logo fiz o Float * fromIntegral Int (que torna se em Flaot e ja da bem)

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x [(a,b)] = a * (x^b)
calcula x ((a,b):p) = a * (x^b) + calcula x p
--calcula 2 [(1,2),(2,1),(3,0)]
-- 11.0

-- rever este ta mal 
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):p) = if b < 1 then simp p else (a,b): simp p 

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((a,b):p) = ((x*a),y+b): mult (x,y) p

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((a,b):(c,d):p) = if b == d then normaliza ((a+c,b):p) else (a,b): normaliza ((c,d):p)  -- ver ta mal

soma :: Polinomio -> Polinomio -> Polinomio 
soma [] [] = []
soma x t = normaliza (x ++ t)

produto :: Polinomio -> Polinomio -> Polinomio
produto _  [] = []
produto [] _ = []
produto (x:y) t = soma (mult x t) (produto y t)
-- basicamente esta funcao faz o produto de dois polinomios 
-- ou seja resulta da soma da multiplicacao da primeira parcela de um polinomio pelo outro
-- e o mesmo com o resto da funcao, ou seja o (produto de y t).

--ordena :: Polinomio -> Polinomio 
--ordena [] = []
--ordena [(a,b)] = [(a,b)]
--ordena ((a,b):(c,d):p) | b < d = (a,b): ordena ((c,d):p)
--                       | b == d = (a,b):(c,d):(ordena p)
--                       | otherwise = (c,d): ordena ((a,b):p) 
-- ta errada porque no primeiro caso ela fica com o (a,b) logo e nada garante que ese seja o menor rever esta funcao

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):p) = ordena (menores p) ++ [(a,b)] ++ ordena (maiores p) 
    where
        menores [] = []
        menores ((c,d):t) = if d < b || ((b == d) && (c < a)) then (c,d): menores t else menores t
        maiores [] = []
        maiores ((c,d):t) = if d > b || ((b == d) && (c >= a)) then (c,d): maiores t else maiores t

equiv :: Polinomio -> Polinomio -> Bool
equiv x y = ordena (normaliza x) == ordena (normaliza y)